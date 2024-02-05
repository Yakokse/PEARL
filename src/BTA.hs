module BTA where
import AST
import Values
import Division
import Utils
import Control.Monad.State

type ST a = State Division a

congruentUniformDiv :: Ord a => Program a () -> Division -> DivisionPW a
congruentUniformDiv (decl, p) d =
  let congruentDiv = makeCongruent (decl, p) d
      ls = labels p
      pairs = map (\l -> (l, congruentDiv)) ls
  in listToPWDiv pairs

makeCongruent :: Program a () -> Division -> Division
makeCongruent p = fixed (execState $ checkProg p)
  where 
    fixed f d' | d' == f d' = d'
               | otherwise = fixed f $ f d'

checkProg :: Program a () -> ST ()
checkProg (decl, p) = mapM_ (checkBlock decl) p

checkBlock :: VariableDecl -> Block a () -> ST ()
checkBlock decl b = 
  do checkFrom $ from b
     mapM_ (checkStep decl) $ body b
     checkJump $ jump b

checkFrom :: ComeFrom a () -> ST ()
checkFrom (Fi e _ _) = do _ <- checkExpr e; return ()
checkFrom _ = return ()

checkJump :: Jump a () -> ST ()
checkJump (If e _ _) = do _ <- checkExpr e; return ()
checkJump _ = return ()

checkStep :: VariableDecl -> Step -> ST ()
checkStep decl (Update n _ e) = 
  do b <- checkExpr e; 
     when b $ setDyn n; 
     when (n `elem` output decl) $ setDyn n
checkStep decl (Replacement q1 q2) = 
  do b1 <- checkPat q1; b2 <- checkPat q2 
     let ns = getVarsPat (QPair q1 q2)
     when (b1 || b2) $ setDyns ns
     when (any (`elem` output decl) ns) $ setDyns ns
checkStep _ (Assert _) = return ()
checkStep _ Skip = return ()

checkPat :: Pattern -> ST Bool
checkPat (QConst _) = return False
checkPat (QVar n) = isDyn n
checkPat (QPair q1 q2) = 
  do b1 <- checkPat q1; b2 <- checkPat q2 
     return (b1 || b2)

checkExpr :: Expr -> ST Bool
checkExpr (Const _)    = return False
checkExpr (Var n)      = isDyn n
checkExpr (Op _ e1 e2) = 
  do b1 <- checkExpr e1; b2 <- checkExpr e2 
     return (b1 || b2)
checkExpr (UOp _ e)    = checkExpr e

setDyn :: Name -> ST ()
setDyn n = do d <- get; put $ setType n BTDynamic d

setDyns :: [Name] -> ST ()
setDyns = mapM_ setDyn

isDyn :: Name -> ST Bool
isDyn n = gets $ isType n BTDynamic 
