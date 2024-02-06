module Explicicator where

import AST2

explicate :: Program' a -> (a -> a) -> Program' a
explicate = undefined