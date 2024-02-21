module Impl.Uniform where
  
import AST
import Values
import Division
import Utils
import Control.Monad.State

type ST a = State Division a

-- Create a congruent uniform division for a program
congruentUniformDiv :: Ord a => NormProgram a -> Division -> DivisionPW a
congruentUniformDiv (decl, p) d =
  let congruentDiv = makeCongruent (decl, p) d
      ls = map nname p
      pairs = map (\l -> (l, (congruentDiv, congruentDiv))) ls
  in listToPWDiv pairs

-- Fix-point iteration for division
makeCongruent :: NormProgram a -> Division -> Division
makeCongruent p = fixed (execState $ checkProg p)
  where 
    fixed f d' | d' == f d' = d'
               | otherwise = fixed f $ f d'

-- Iterate through whole program
-- The Division is preserved in the state-monad for fewer arguments passed
-- and the division will grow increasingly dynamic throughout the iteration
checkProg :: NormProgram a -> ST ()
checkProg (decl, p) = mapM_ (checkBlock decl) p

-- Iterate through block
-- Only check step as control flow does not affect BTA
-- (They would only check expression which has no effect)
checkBlock :: VariableDecl -> NormBlock a -> ST ()
checkBlock decl b = checkStep decl $ nstep b

-- Make division congruent for given step
checkStep :: VariableDecl -> Step -> ST ()
checkStep decl (Update n _ e) = 
  do b <- isDynExpr e; 
     when b $ setDyn n; 
     when (n `elem` output decl) $ setDyn n
checkStep decl (Replacement q1 q2) = 
  do b1 <- isDynPat q1; b2 <- isDynPat q2 
     let ns = getVarsPat (QPair q1 q2)
     when (b1 || b2) $ setDyns ns
     when (any (`elem` output decl) ns) $ setDyns ns
checkStep _ (Assert _) = return ()
checkStep _ Skip = return ()

-- Is a given pattern at least partially dynamic under division
isDynPat :: Pattern -> ST Bool
isDynPat (QConst _) = return False
isDynPat (QVar n) = isDynVar n
isDynPat (QPair q1 q2) = 
  do b1 <- isDynPat q1; b2 <- isDynPat q2 
     return (b1 || b2)

-- Is the expression at least partially dynamic under division
isDynExpr :: Expr -> ST Bool
isDynExpr (Const _)    = return False
isDynExpr (Var n)      = isDynVar n
isDynExpr (Op _ e1 e2) = 
  do b1 <- isDynExpr e1; b2 <- isDynExpr e2 
     return (b1 || b2)
isDynExpr (UOp _ e)    = isDynExpr e

-- Make a variable dynamic
setDyn :: Name -> ST ()
setDyn n = do d <- get; put $ setType n BTDynamic d

-- Make a list of variables dynamic
setDyns :: [Name] -> ST ()
setDyns = mapM_ setDyn

-- Check if a variable is dynamic
isDynVar :: Name -> ST Bool
isDynVar n = gets $ isType n BTDynamic 
