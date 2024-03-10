module Assertions.Removal where

import RL.AST

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

removeAssertions :: Program a b -> Program a b
removeAssertions = undefined
