module Assertions.Analysis where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Values

import Assertions.Abstraction

import qualified Data.List as List
import Control.Monad (foldM)
import Control.Applicative (Applicative(liftA2))

-- Maybe AStore forms a lattice
-- If any var is bot, whole astore is bot
type AStore = Map Name AValue
type State a b = Map (a,b) (Maybe (AStore, AStore))

-- Fix-point iteration for state
inferProg :: (Ord a, Ord b) => State a b -> [Block a b]
                            -> [(a,b)]   -> State a b
inferProg state _ [] = state
inferProg state prog (l:ls) =
  let b = getBlockUnsafe prog l
      (state', pending) = inferBlock state prog b
      ls' = List.union ls pending
  in inferProg state' prog ls'

-- Update state and return new pending points
inferBlock :: (Ord a, Ord b) => State a b -> [Block a b] -> Block a b
                             -> (State a b, [(a, b)])
inferBlock state prog
           Block{name = n, from = f, body = b, jump = j} =
    let parents = map (inferTransition state prog n) $ fromLabels f
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

inferTransition :: (Ord a, Ord b) => State a b -> [Block a b] -> (a, b) -> (a, b)
                                  -> Maybe AStore
inferTransition state prog dest orig =
  do let origJump = jump $ getBlockUnsafe prog orig
     (_, origState) <- get orig state
     case origJump of
       If e l1 l2 | l1 == dest && l2 /= dest ->
         inferAssertion origState e
       If e l1 l2 | l1 /= dest && l2 == dest ->
         inferAssertion origState (UOp Not e)
       _ -> return origState

-- glb in AStore lattice
glbStore :: Maybe (AStore, AStore) -> Maybe (AStore, AStore) -> Maybe (AStore, AStore)
glbStore Nothing _ = Nothing
glbStore _ Nothing = Nothing
glbStore (Just (s1, s2)) (Just (s1', s2')) =
  let s1'' = combineWith aglb s1 s1'
      s2'' = combineWith aglb s2 s2'
  in do ms1 <- sequence s1''
        ms2 <- sequence s2''
        return (ms1, ms2)

-- lub in AStore lattice
lubStore :: Maybe AStore -> Maybe AStore -> Maybe AStore
lubStore Nothing s = s
lubStore s Nothing = s
lubStore (Just s1) (Just s2) = return $ combineWith alub s1 s2

-- Infer new store from step and store
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

-- Construct abstract value and store from pattern
inferConstruct :: AStore -> Pattern -> (AValue, AStore)
inferConstruct s (QConst v) = (aValue v, s)
inferConstruct s (QVar n) = (get n s, set n ANil s)
inferConstruct s (QPair q1 q2) =
  let (av1, s'') = inferConstruct s   q1
      (av2, s')  = inferConstruct s'' q2
  in (APair av1 av2, s')

-- Deconstruct the abstract value, fails if impossible
inferDeconstruct :: AStore -> Pattern -> AValue -> Maybe AStore
inferDeconstruct s (QConst v) av =
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
          ANonNil -> return (Any, Any)
          _ -> Nothing
     s' <- inferDeconstruct s q1 av1
     inferDeconstruct s' q2 av2

-- Infer the abstract value of an expression
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

-- Infer from assertion/transition
-- We can't use it for assertions as we want to detect when they are redundant
inferAssertion :: AStore -> Expr -> Maybe AStore
inferAssertion s (Var n) =
  let av = get n s
  in case av `aglb` ANonNil of
      Just v -> return $ set n v s
      Nothing -> Nothing
inferAssertion s (Op And e1 e2) =
  do s1 <- inferAssertion s e1
     s2 <- inferAssertion s e2
     sequence $ combineWith aglb s1 s2
inferAssertion s (Op Or e1 e2) =
  let ms1 = inferAssertion s e1
      ms2 = inferAssertion s e2
  in lubStore ms1 ms2
inferAssertion s (UOp Not e) =
  case e of
    (Var n) ->
      let av = get n s
      in case av `aglb` ANil of
          Just v -> return $ set n v s
          Nothing -> Nothing
    (UOp Not e') -> inferAssertion s e'
    (Op And e1 e2) -> inferAssertion s (Op Or  (UOp Not e1) (UOp Not e2))
    (Op Or e1 e2)  -> inferAssertion s (Op And (UOp Not e1) (UOp Not e2))
    _ -> return s
inferAssertion s _ = return s
