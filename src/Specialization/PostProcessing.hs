module Specialization.PostProcessing
  ( constFold
  , mergeExplicators
  , mergeExits
  , removeDeadBlocks
  , changeConditionals
  , compressPaths
  , enumerateAnn) where

import Specialization.Impl.PostProcessing
