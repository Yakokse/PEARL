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
  let final = fixpoint initS
      result = map (removeAssertionsBlock final) p
  in (decl, result)
  where
    entry = getEntryName p
    (_, pInv) = invertProg (decl, p)
    exit = getEntryName pInv
    initS = initState (decl, p)
    fixpoint state =
      let state1 = inferProg state p [entry]
          state2 = inferProg (invertState state) pInv [exit]
          newState = combineWith glbStore state1 (invertState state2)
      in if state == newState then state else fixpoint newState

invertState :: State a b -> State a b
invertState = mmap . const $ fmap (\(a, b) -> (b,a))

-- todo: logging
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
                  _ -> []
      in (return s, res)
    iterStep (Just s) step = (inferStep s step, [step])

initState :: (Ord a, Ord b) => Program a b -> State a b
initState (decl, p) =
  let entry = getEntryName p
      exit = getExitName p
      bottomState = fromList $ map (\b -> (name b, Nothing)) p
  in if entry /= exit
    then set exit finalElem $
         set entry initElem bottomState
    else set entry singleElem bottomState
  where
    vars = allVars decl
    allAny = fromList $ map (\n -> (n, Any)) vars
    initStore = sets (nonInput decl) ANil allAny
    finalStore = sets (nonOutput decl) ANil allAny
    initElem = return (initStore, emptyMap)
    finalElem = return (emptyMap, finalStore)
    singleElem = return (initStore, finalStore)
