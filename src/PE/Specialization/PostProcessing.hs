module PE.Specialization.PostProcessing
  ( constFold
  , mergeExplicators
  , mergeExits
  , removeDeadBlocks
  , changeConditionals
  , compressPaths
  , enumerateAnn) where

import PE.Specialization.Impl.PostProcessing
