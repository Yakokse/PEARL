module Normalize where

import AST
import Utils

type NormProgram label = (VariableDecl, [NormBlock label])
data NormBlock label = NormBlock 
  { nname :: (label, Int) 
  , nfrom :: ComeFrom (label, Int) ()
  , nstep :: Step 
  , njump :: Jump (label, Int) ()
  }

denormalize :: (a -> Int -> a) -> NormProgram a -> Program a () 
denormalize f (decl, np) = (decl, map denormBlock np)
  where
    f' ((a, i), ()) = (f a i, ())
    denormBlock NormBlock{ nname = n
                         , nfrom = k
                         , nstep = s
                         , njump = j} =
      Block { name = (uncurry f n, ())
            , from = mapFrom f' k
            , body = [s]
            , jump = mapJump f' j
            }

normalize :: Eq a => Program a () -> NormProgram a
normalize (decl, prog) = (decl, concatMap (normalizeBlock prog) prog)

normalizeBlock :: Eq a => [Block a ()] -> Block a () -> [NormBlock a]
normalizeBlock prog Block{name = (l, ()), from = k, body = b, jump = j} = 
  let b' = if null b then [Skip] else b
  in zipWith normalizeStep b' [1..] 
  where
    normalizeStep step n = 
      let k' = if n == 1 then normalizeFrom prog k else From ((l, n-1), ())
          j' = if n == length b then normalizeJump j else Goto ((l, n+1), ())
      in NormBlock (l,n) k' step j'

normalizeFrom :: (Eq a, Eq b) => [Block a b] -> ComeFrom a b -> ComeFrom (a, Int) b
normalizeFrom prog k = 
  case k of
    Entry s -> Entry s
    From l -> From $ transformLab l
    Fi e l1 l2 -> Fi e (transformLab l1) (transformLab l2)
  where 
    length' l = if null l then 1 else length l
    transformLab (l, s) = 
      let b = getBlockUnsafe prog (l, s)
      in ((l, length' $ body b), s)

normalizeJump :: Jump a b -> Jump (a, Int) b
normalizeJump (Exit s) = Exit s
normalizeJump (Goto (l,s)) = Goto ((l,1), s)
normalizeJump (If e (l1, s1) (l2, s2)) = If e ((l1,1), s1) ((l2,1), s2)
