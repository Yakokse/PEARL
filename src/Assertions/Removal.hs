module Assertions.Removal (removeAssertions) where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Variables

import Assertions.Abstraction
import Assertions.Analysis

import Inversion.Inverter

-- todo: propagate inverted state for analysis properly
removeAssertions :: (Ord a, Ord b) => Program a b -> Program a b
removeAssertions (decl, p) =
  let initS = initState (decl, p)
      final = fixpoint initS
      result = map (removeAssertionsBlock final) p
  in (decl, result)
  where
    entry = getEntryName p
    (declInv, pInv) = invertProg (decl, p)
    exit = getEntryName pInv
    exitS = initState (declInv, pInv)
    fixpoint state =
      let state1 = inferProg state p [entry]
          state2 = invertState $ inferProg exitS pInv [exit]
          newState = combineWith (\(s1,s2) (s1', s2') ->
                        (combineWith aglb s1 s1', combineWith aglb s2 s2'))
                        state1 state2
      in if state == newState then state else fixpoint newState

invertState :: State a b -> State a b
invertState = mmap $ const (\(a, b) -> (b,a))

removeAssertionsBlock :: (Ord a, Ord b) => State a b -> Block a b
                                        -> Block a b
removeAssertionsBlock state b =
  let aStore = fst . flip get state $ name b
      cleaned = foldl (\(s, acc) step ->
                          let (s', step') = iterStep s step
                          in (s', acc ++ step')) (aStore, [])
      body' = snd . cleaned $ body b
  in b{body = body'}
  where
    iterStep s (Assert e) =
      let av = inferExpr s e
          res =  [Assert e | canNil av]
      in (s, res)
    iterStep s step = (inferStep s step, [step])

initState :: (Ord a, Ord b) => Program a b -> State a b
initState (decl, p) =
  let entry = getEntryName p
      vars = allVars decl
      bottomStore = fromList $ map (\n -> (n, ANil)) vars
      bottomState = fromList $ map (\b -> (name b, (bottomStore, bottomStore))) p
      initStore = sets (nonInput decl) ANil bottomStore
  in set entry (initStore, bottomStore) bottomState
