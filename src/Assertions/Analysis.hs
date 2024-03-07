module Assertions.Analysis where

-- import Assertions.Abstraction
-- import Inversion.Inverter

-- AbstractStore :: Name -> AValue
-- State = PW AbstractStore :: Label -> (StartStore, EndStore)

-- Workqueue
-- initial stores: entry start = input = any, non-input = nil
--                 exit end = output = any, non-output = nil
--                 all others: everything bottom
-- if anything changes in output, work on children
-- work through list of blocks to analyse
-- swap around and check if things change? if so, repeat

-- analyse a block:
-- Startstore = (lub NONEAWARE) of parents endstore (and glb? current start)
-- fold over the steps for new endstore
-- lub with old endstore

-- after fixpoint reached: remove assertions when trivial
--                         blocks when trivial false assert / none

-- thought: operate on normalized or not, easier postproc but annoying preproc
