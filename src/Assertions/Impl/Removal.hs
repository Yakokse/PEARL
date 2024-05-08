module Assertions.Impl.Removal where

import Utils.Maps

import RL.AST
import RL.Values
import RL.Variables

import Assertions.Impl.Abstraction
import Assertions.Impl.Analysis

import Inversion.Inverter
import PE.Specialization.PostProcessing

import Data.Maybe (maybeToList)
import Control.Monad (foldM)

removeAllAssertions :: (Ord a, Ord b) => Program a b -> Program a b
removeAllAssertions = removeAssertionsUni . removeAssertionsBi

-- Detect and remove redundant assertions in a given program
removeAssertionsUni :: (Ord a, Ord b) => Program a b -> Program a b
removeAssertionsUni prog =
  let preState = initPreState prog
      state1 = inferProg prog preState
      cleaned1 = removeAssertionsProg (postToPre prog state1) prog
      prog2 = invertProg cleaned1
      state2 = inferProg prog2 preState
      cleaned2 = removeAssertionsProg (postToPre prog2 state2) prog2
  in invertProg cleaned2

removeAssertionsBi :: (Ord a, Ord b) => Program a b -> Program a b
removeAssertionsBi prog =
  let preState = initPreState prog
      final = fixpoint preState
      result = removeAssertionsProg final prog
  in result
  where
    invProg = invertProg prog
    fixpoint preState =
     let postState = inferProgWithoutAsserts prog preState
         preState' = inferProgWithoutAsserts invProg postState
     in if preState == preState'
        then preState
        else fixpoint preState'

postToPre :: (Ord a, Ord b) => Program a b -> State a b
                            -> State a b
postToPre prog state =
  let preStore b = inferFrom state prog (name b) (from b)
      preStores = map (\b -> (name b, preStore b)) $ snd prog
  in fromList preStores

removeAssertionsProg :: (Ord a, Ord b) => State a b -> Program a b
                                       -> Program a b
removeAssertionsProg preState (decl, pblocks) =
  let cleanBlock b =
        let initStore = get (name b) preState
        in maybeToList $ removeAssertionsBlock initStore b
      cleaning = compressPaths .
                 changeConditionals .
                 removeDeadBlocks .
                 concatMap cleanBlock
      cleanBody = cleaning . cleaning $ pblocks
  in (decl, cleanBody)

-- Remove all redundant assertions in a given block
-- Fold over all steps so we don't need to work on normalized blocks
removeAssertionsBlock :: Maybe AStore -> Block a b -> Maybe (Block a b)
removeAssertionsBlock Nothing _ = Nothing
removeAssertionsBlock (Just startStore) b =
  let remove = foldM (\(store, acc) step ->
                          do (store', step') <- iterStep store step
                             return (store', acc ++ step'))
                      (startStore, [])
      updateBlock (s, steps) =
        let (j, js) = iterJump s $ jump b
        in return b{body = steps ++ js, jump = j}
  in remove (body b) >>= updateBlock
  where
    iterJump s (If e l1 l2)
      | inferAssertion s e == Nothing
          = (Goto l2, [Assert (UOp Not e)])
      | inferAssertion s (UOp Not e) == Nothing
          = (Goto l1, [Assert e])
      | otherwise = (If e l1 l2, [])
    iterJump _ j = (j, [])
    iterStep s (Assert e) =
      case reduceExpr s e of
          Just (_, ANil) -> Nothing
          Just (e', v) ->
            do s' <- inferAssertion s e
               let res = [Assert e' | v `aglb` ANil /= Nothing]
               return (s', res)
          Nothing -> Nothing -- Bottom case
    iterStep s step = do s' <- inferStep s step
                         return (s', [step])

reduceExpr :: AStore -> Expr -> Maybe (Expr, AValue)
reduceExpr _ (Const v) = return (Const v, aValue v)
reduceExpr s (Var n)   = return (Var n, get n s)
reduceExpr s (UOp op e) =
  do (e', v) <- reduceExpr s e
     v' <- aUnOp op v
     let e'' = case (op, e') of
                (Hd, Op Cons e1 _) -> e1
                (Tl, Op Cons _ e2) -> e2
                (Not, _) | v `lteStrict` ANil    -> Const trueV
                (Not, _) | v `lteStrict` ANonNil -> Const Nil
                (Not, UOp Not e1)                -> e1
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
              Or  | v1 `lteStrict` ANil    -> e2
              Or  | v2 `lteStrict` ANil    -> e1
              _ -> Op op e1' e2'
     return (e, v)

initPreState :: (Ord a, Ord b) => Program a b -> State a b
initPreState (decl, prog) =
  let anyStore = fromList $ map (\n -> (n, Any)) $ allVars decl
      preState = fromList $ map (\b -> (name b, Just anyStore)) prog
  in preState
