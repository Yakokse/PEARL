module Impl.Normalize where

import AST
import Utils

-- normalize a program
normalize :: Eq a => a -> a -> Program a () -> (a -> Int -> a) -> NormProgram a
normalize entry exit (decl, prog) f = (decl, pad entry exit $ concatMap (normalizeBlock prog f) prog)

-- add an extra nop block in the beginning of a program
-- for room for explicator in beginning of program
pad :: Eq a => a -> a -> [NormBlock a] -> [NormBlock a]
pad entryLabel exitLabel prog = 
  let entryBlock = getNEntryBlock prog
      exitBlock = getNExitBlock prog
      l1 = nname entryBlock
      l2 = nname exitBlock
      padding1 = NormBlock entryLabel (Entry ()) Skip (Goto (l1, ()))
      padding2 = NormBlock exitLabel (From (l2, ())) Skip (Exit ())
  in if l1 /= l2
     then let rest = filter (\b -> nname b `notElem` [l1, l2]) prog
              entry' = entryBlock{nfrom = From (entryLabel, ())}
              exit' = exitBlock{njump = Goto (exitLabel, ())}
          in [padding1, entry'] ++ rest ++ [exit', padding2]
     else [ padding1
          , exitBlock{ nfrom = From (entryLabel, ())
                     , njump = Goto (entryLabel, ())
                     }
          , padding2]

-- normalize a block, transforming each step into its own block
normalizeBlock :: Eq a => [Block a ()] -> (a -> Int -> a) -> Block a () -> [NormBlock a]
normalizeBlock prog f Block{name = (l, ()), from = k, body = b, jump = j} = 
  zipWith normalizeStep b' [1..] 
  where
    b' = if null b then [Skip] else b
    normalizeStep step n = 
      let k' = if n == 1 then normalizeFrom prog f k else From (f l (n-1), ())
          j' = if n == length b' then normalizeJump f j else Goto (f l (n+1), ())
      in NormBlock (f l n) k' step j'

-- point come-from labels to the correct normalized block
normalizeFrom :: (Eq a, Eq b) => [Block a b] -> (a -> Int -> a) -> ComeFrom a b -> ComeFrom a b
normalizeFrom prog f k = 
  case k of
    Entry s -> Entry s
    From l -> From $ transformLab l
    Fi e l1 l2 -> Fi e (transformLab l1) (transformLab l2)
  where 
    length' l = if null l then 1 else length l
    transformLab (l, s) = 
      let b = getBlockUnsafe prog (l, s)
      in (f l (length' $ body b), s)

-- point jump labels to correct normalized block
normalizeJump :: (a -> Int -> a) -> Jump a b -> Jump a b
normalizeJump _ (Exit s) = Exit s
normalizeJump f (Goto (l,s)) = Goto (f l 1, s)
normalizeJump f (If e (l1, s1) (l2, s2)) = If e (f l1 1, s1) (f l2 1, s2)
