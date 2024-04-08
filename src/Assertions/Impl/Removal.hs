module Assertions.Impl.Removal where

import Utils.Maps

import RL.AST
import RL.Values

import Assertions.Impl.Abstraction
import Assertions.Impl.Analysis

import Inversion.Inverter

-- Detect and remove redundat assertions in a given program
removeAssertions :: (Ord a, Ord b) => Program a b -> Program a b
removeAssertions prog =
  let state1 = inferProg prog
      cleaned1 = removeAssertionsProg state1 prog
      prog2 = invertProg cleaned1
      state2 = inferProg prog2
      cleaned2 = removeAssertionsProg state2 prog2
  in invertProg cleaned2

removeAssertionsProg :: (Ord a, Ord b) => State a b -> Program a b
                                       -> Program a b
removeAssertionsProg state prog@(decl, pbody) =
  let startStore b = inferTransition state prog (name b) (from b)
      startStores = map startStore pbody
      cleanBody = zipWith removeAssertionsBlock startStores pbody
  in (decl, cleanBody)

-- todo: logging, assertion PE
-- Remove all redundant assertions in a given block
-- Fold over all steps so we don't need to work on normalized blocks
removeAssertionsBlock :: Maybe AStore -> Block a b -> Block a b
removeAssertionsBlock startStore b =
  let cleaned = foldl (\(store, acc) step ->
                          let (store', step') = iterStep store step
                          in (store', acc ++ step'))
                      (startStore, [])
      body' = snd . cleaned $ body b
  in b{body = body'}
  where
    iterStep Nothing s = (Nothing, [s])
    iterStep (Just s) (Assert e) =
      let res = case reduceExpr s e of
                  Just (e', v) -> [Assert e' | v `aglb` ANil /= Nothing]
                  Nothing -> [Assert e] -- Bottom case
      in (return s, res)
    iterStep (Just s) step = (inferStep s step, [step])

-- Either Expr AValue?
-- AValue for top -> Expr?
-- Bot in ops -> Expr?
reduceExpr :: AStore -> Expr -> Maybe (Expr, AValue)
reduceExpr _ (Const v) = return (Const v, aValue v)
reduceExpr s (Var n)   = return (Var n, get n s)
  -- let v = get n s
  -- in if v `aglb` ANil /= Nothing
  --   then return $ Var n
  --   else Nothing
reduceExpr s (UOp op e) =
  do (e', v) <- reduceExpr s e
     v' <- aUnOp op v
     let e'' = case (op, e') of
                (Hd, Op Cons e1 _) -> e1
                (Tl, Op Cons _ e2) -> e2
                (Not, UOp Not e1)  -> e1
                _ -> UOp op e'
     return (e'', v')
reduceExpr s (Op op e1 e2) =
  do (e1', v1) <- reduceExpr s e1
     (e2', v2) <- reduceExpr s e2
     v <- aBinOp op v1 v2
     let e = case op of
              And | v1 `lteStrict` ANonNil -> e2
              And | v2 `lteStrict` ANonNil -> e1
              And | v1 `lteStrict` ANil    -> Const Nil
              And | v2 `lteStrict` ANil    -> Const Nil
              Or  | v1 `lteStrict` ANonNil -> e1
              Or  | v2 `lteStrict` ANonNil -> e2
              Or  | v1 `lteStrict` ANil    -> e2
              Or  | v2 `lteStrict` ANil    -> e1
              _ -> Op op e1' e2'
     return (e, v)
