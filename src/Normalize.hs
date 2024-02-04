module Normalize where

import AST
import Utils

normalizeWith :: (Eq a, Eq b) => (a -> Int -> a) -> Program a b -> Program a b
normalizeWith f p = 
  let (decl, prog) = normalize p
  in (decl, mapLabel (uncurry f) prog) 

normalize :: (Eq a, Eq b) => Program a b -> Program (a, Int) b
normalize (decl, prog) = (decl, concatMap (normalizeBlock prog) prog)

normalizeBlock :: (Eq a, Eq b) =>  [Block a b] -> Block a b -> [Block (a, Int) b]
normalizeBlock prog Block{name = (l, s), from = k, body = b, jump = j} = 
  if null b
  then [Block ((l,1), s) (normalizeFrom prog k) b (normalizeJump j)]
  else zipWith normalizeStep b [1..] 
  where
    normalizeStep step n = 
      let k' = if n == 1 then normalizeFrom prog k else From ((l, n-1), s)
          j' = if n == length b then normalizeJump j else Goto ((l, n+1), s)
      in Block ((l,n), s) k' [step] j'

normalizeFrom :: (Eq a, Eq b) => [Block a b] -> ComeFrom a b -> ComeFrom (a, Int) b
normalizeFrom prog k = 
  case k of
    Entry s -> Entry s
    From l -> From $ transformLab l
    Fi e l1 l2 -> Fi e (transformLab l1) (transformLab l2)
  where 
    transformLab (l, s) = 
      let b = getBlockUnsafe prog (l, s)
      in ((l, length $ body b), s)

normalizeJump :: Jump a b -> Jump (a, Int) b
normalizeJump (Exit s) = Exit s
normalizeJump (Goto (l,s)) = Goto ((l,1), s)
normalizeJump (If e (l1, s1) (l2, s2)) = If e ((l1,1), s1) ((l2,1), s2)
