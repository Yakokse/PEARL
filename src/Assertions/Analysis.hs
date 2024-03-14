module Assertions.Analysis where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Values

import Assertions.Abstraction

import qualified Data.List as List
import Control.Monad (foldM)
import Control.Applicative (Applicative(liftA2))

type AStore = Map Name AValue
type State a b = Map (a,b) (Maybe (AStore, AStore))

inferProg :: (Ord a, Ord b) => State a b -> [Block a b]
                            -> [(a,b)]   -> State a b
inferProg state _ [] = state
inferProg state prog (l:ls) =
  let b = getBlockUnsafe prog l
      (state', pending) = inferBlock state b
      ls' = List.union ls pending
  in inferProg state' prog ls'

inferBlock :: (Ord a, Ord b) => State a b -> Block a b
                             -> (State a b, [(a, b)])
inferBlock state
           Block{name = n, from = f, body = b, jump = j} =
    let parents = map (\n' -> snd <$> get n' state) $ fromLabels f
        mOrigs = get n state
        origS1 = fst <$> mOrigs
        origS2 = snd <$> mOrigs
        initStore = foldl lubStore origS1 parents
        outStore = case initStore of
                    Just s -> foldM inferStep s b
                    Nothing -> Nothing
        pending = if origS2 == outStore then []
                      else jumpLabels j
        resultPair = liftA2 (,) initStore outStore
        newState = set n resultPair state
    in (newState, pending)

glbStore :: Maybe (AStore, AStore) -> Maybe (AStore, AStore) -> Maybe (AStore, AStore)
glbStore Nothing _ = Nothing
glbStore _ Nothing = Nothing
glbStore (Just (s1, s2)) (Just (s1', s2')) =
  let s1'' = combineWith aglb s1 s1'
      s2'' = combineWith aglb s2 s2'
  in do ms1 <- sequence s1''
        ms2 <- sequence s2''
        return (ms1, ms2)

lubStore :: Maybe AStore -> Maybe AStore -> Maybe AStore
lubStore Nothing s = s
lubStore s Nothing = s
lubStore (Just s1) (Just s2) = return $ combineWith alub s1 s2

inferStep :: AStore -> Step -> Maybe AStore
inferStep s Skip = return s
inferStep s (Assert _) = return s -- Assertions do convey information, but we need to ignore it
inferStep s (Update n op e) =
  do let av1 = get n s
     av2 <- inferExpr s e
     av3 <- aRevOp op av1 av2
     return $ set n av3 s
inferStep s (Replacement q1 q2) =
  let (av, s'') = inferConstruct s q2
  in inferDeconstruct s'' q1 av

inferConstruct :: AStore -> Pattern -> (AValue, AStore)
inferConstruct s (QConst v) = (aValue v, s)
inferConstruct s (QVar n) = (get n s, set n ANil s)
inferConstruct s (QPair q1 q2) =
  let (av1, s'') = inferConstruct s   q1
      (av2, s')  = inferConstruct s'' q2
  in (APair av1 av2, s')

inferDeconstruct :: AStore -> Pattern -> AValue -> Maybe AStore
inferDeconstruct s (QConst v) av = -- How to handle mistake in deconstruct
  do assert $ canEqual (aValue v) av
     return s
inferDeconstruct s (QVar n) av =
  do let av1 = get n s
     assert $ canNil av1
     return $ set n av s
inferDeconstruct s (QPair q1 q2) av =
  do (av1, av2) <-
        case av of
          APair av1' av2' -> return (av1', av2')
          Any -> return (Any, Any)
          _ -> Nothing
     s' <- inferDeconstruct s q1 av1
     inferDeconstruct s' q2 av2

inferExpr :: AStore -> Expr -> Maybe AValue
inferExpr _ (Const v) = return $ aValue v
inferExpr s (Var n) = return $ get n s
inferExpr s (Op op e1 e2) =
  do av1 <- inferExpr s e1
     av2 <- inferExpr s e2
     aBinOp op av1 av2
inferExpr s (UOp op e) =
  do av <- inferExpr s e
     aUnOp op av
