module Assertions.Removal (removeAssertions) where

import Utils.Maps

import RL.AST
import RL.Program
import RL.Variables

import Assertions.Abstraction
import Assertions.Analysis

-- Workqueue
-- initial stores: entry start = input = any, non-input = nil
--                 exit end = output = any, non-output = nil
--                 all others: everything bottom
-- if anything changes in output, work on children
-- work through list of blocks to analyse
-- swap around and check if things change? if so, repeat
-- plan start from both paths, no clever intermingling

-- analyse a block:
-- Startstore = (lub NONEAWARE) of parents endstore (and glb? current start)
-- fold over the steps for new endstore
-- lub with old endstore -- doesnt help with reflection plan

-- after fixpoint reached: remove assertions when trivial
--                         blocks when trivial false assert / none

removeAssertions :: (Ord a, Ord b) => Program a b -> Program a b
removeAssertions (decl, p) =
  let initS = initState (decl, p)
      entry = getEntryName p
      final = inferProg initS p [entry]
      result = map (removeAssertionsBlock final) p
  in (decl, result)

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
