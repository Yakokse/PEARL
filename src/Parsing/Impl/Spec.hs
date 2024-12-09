module Parsing.Impl.Spec where

import Utils.Error

import RL.Values

import Parsing.Impl.Common


-- parse a string as a program input
parseSpec :: String -> EM Value
parseSpec = parseStr pFile
  where pFile = pConstant