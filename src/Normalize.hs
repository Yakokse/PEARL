module Normalize (normalizeProgram) where

import RL.AST
import RL.Program

import Data.List (sort, union, elemIndex)


normalizeProgram :: Eq a => Program a () -> Program String ()
normalizeProgram (decl, prog) = (normDecl decl, normBlocks prog)

normDecl :: VariableDecl -> VariableDecl
normDecl VariableDecl{ input = inp, output = out, temp = tmp} =
  VariableDecl (sort inp) (sort out) (sort tmp)

normBlocks :: Eq a => [Block a ()] -> [Block String ()]
normBlocks bs =
  let sorted = topologicalSort bs
      order = map label sorted
  in mapLabel (anonymizeLabel order) sorted

anonymizeLabel :: Eq a => [a] -> a -> String
anonymizeLabel labels l =
  case elemIndex l labels of
    Nothing -> error "Label not found in given order"
    Just i -> "Block_" ++ show i

topologicalSort :: Eq a => [Block a ()] -> [Block a ()]
topologicalSort bs =
  let entry = getEntryLabel bs
  in topSortAcc [(entry, ())] []
  where
    topSortAcc [] acc = map (getBlockUnsafe bs) acc
    topSortAcc (l:pending) acc =
      let newLs = jumpLabels . jump $ getBlockUnsafe bs l
          newAcc = if l `elem` acc then acc else acc ++ [l]
          newPending = union pending $ filter (`notElem` acc) newLs
      in topSortAcc newPending newAcc
