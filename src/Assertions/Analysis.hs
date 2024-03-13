module Assertions.Analysis where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Values

import Assertions.Abstraction

import qualified Data.List as List

type AStore = Map Name AValue
type State a b = Map (a,b) (AStore, AStore)

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
    let parents = map (\n' -> snd $ get n' state) $ fromLabels f
        (origS1, origS2) = get n state
        initStore = foldl lubStore origS1 parents
        outStore = foldl inferStep initStore b
        pending = if origS2 == outStore then []
                  else jumpLabels j
        newState = set n (initStore, outStore) state
    in (newState, pending)

lubStore :: AStore -> AStore -> AStore
lubStore s1 s2
  | anyWhere (const (== ANil)) s1 = s2
  | anyWhere (const (== ANil)) s2 = s1
  | otherwise = combineWith alub [s1, s2]

inferStep :: AStore -> Step -> AStore
inferStep s Skip = s
inferStep s (Assert _) = s -- Assertions do convey information
inferStep s (Update n op e) =
  let av1 = get n s
      av2 = inferExpr s e
      av3 = aRevOp op av1 av2
  in set n av3 s
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

inferDeconstruct :: AStore -> Pattern -> AValue -> AStore
inferDeconstruct s (QConst v) av = -- How to handle mistake in deconstruct
  if av == aValue v then s else undefined
inferDeconstruct s (QVar n) av =
  let av1 = get n s
  in if canNil av1 then set n av s
     else set n None s -- error, set to None?
inferDeconstruct s (QPair q1 q2) av =
  let (av1, av2) =
        case av of
          APair av1' av2' -> (av1', av2')
          Any -> (Any, Any)
          _ -> (None, None) -- error, set vars to None (none, none)?
      s' = inferDeconstruct s q1 av1
  in inferDeconstruct s' q2 av2

inferExpr :: AStore -> Expr -> AValue
inferExpr _ (Const v) = aValue v
inferExpr s (Var n) = get n s
inferExpr s (Op op e1 e2) =
  let av1 = inferExpr s e1
      av2 = inferExpr s e2
  in aBinOp op av1 av2
inferExpr s (UOp op e) =
  aUnOp op $ inferExpr s e
