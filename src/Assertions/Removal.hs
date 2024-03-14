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
  let final = fixpoint initS exitS
      result = map (removeAssertionsBlock final) p
  in (decl, result)
  where
    entry = getEntryName p
    (declInv, pInv) = invertProg (decl, p)
    exit = getEntryName pInv
    initS = initState (decl, p)
    exitS = initState (declInv, pInv)
    fixpoint state stateInv =
      let state1 = inferProg state p [entry]
          state2 = inferProg stateInv pInv [exit]
          newState = combineWith glbStore state1 (invertState state2)
      in if state == newState then state else fixpoint newState (invertState newState)

invertState :: State a b -> State a b
invertState = mmap . const $ fmap (\(a, b) -> (b,a))


-- remove blocks with statically enforced errors?
removeAssertionsBlock :: (Ord a, Ord b) => State a b -> Block a b
                                        -> Block a b
removeAssertionsBlock state b =
  let aStore = fst <$> get (name b) state
      cleaned = foldl (\(s, acc) step ->
                          let (s', step') = iterStep s step
                          in (s', acc ++ step')) (aStore, [])
      body' = snd . cleaned $ body b
  in b{body = body'}
  where
    iterStep Nothing s = (Nothing, [s])
    iterStep (Just s) (Assert e) =
      let res = case inferExpr s e of
                  Just av | canNil av -> [Assert e]
                  Just _              -> []
                  _ -> [Assert e]
      in (return s, res)
    iterStep (Just s) step = (inferStep s step, [step])

initState :: (Ord a, Ord b) => Program a b -> State a b
initState (decl, p) =
  let entry = getEntryName p
      vars = allVars decl
      bottomState = fromList $ map (\b -> (name b, Nothing)) p
      allAny = fromList $ map (\n -> (n, Any)) vars
      initStore = sets (nonInput decl) ANil allAny
      initElem = return (initStore, emptyMap)
  in set entry initElem bottomState
