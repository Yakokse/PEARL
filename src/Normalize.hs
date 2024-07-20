module Normalize (normalizeProgram) where

import RL.AST
import RL.Program

import Data.List (sort, union, elemIndex)

normalizeProgram :: Eq a => Program a () -> Program String ()
normalizeProgram (decl, prog) = (normDecl decl, normBlocks prog)

-- Sort the variables because why not, it should not matter
normDecl :: VariableDecl -> VariableDecl
normDecl VariableDecl{input = inp, output = out, temp = tmp} =
  VariableDecl (sort inp) (sort out) (sort tmp)

-- Make the label order and names uniform
normBlocks :: Eq a => [Block a ()] -> [Block String ()]
normBlocks bs =
  let sorted = topologicalSort bs
      order = map label sorted
  in mapLabel (anonymizeLabel order) sorted

-- Convert label to string using index in list
anonymizeLabel :: Eq a => [a] -> a -> String
anonymizeLabel labels l =
  case elemIndex l labels of
    Nothing -> error "Label not found in given order"
    Just i -> "Block_" ++ show i

-- Sort the blocks using control flow (entry block is first, etc.)
topologicalSort :: Eq a => [Block a ()] -> [Block a ()]
topologicalSort bs =
  let entry = getEntryLabel bs
  in topSortAcc [(entry, ())] []
  where
    topSortAcc [] acc = map (getBlockUnsafe bs) acc
    topSortAcc (l:pending) acc =
      let newLs = jumpLabels . jump $ getBlockUnsafe bs l
          (pending', acc') = if l `elem` acc then (pending, acc)
                             else (pending `union` newLs, acc ++ [l])
      in topSortAcc pending' acc'
