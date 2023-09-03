module BTA where
import AST
import Values
import Division
import Control.Monad.State

type ST a = State Division a

-- TODO: Expr: static a[dydnamic e] needs detection, time for a state monad prob
makeCongruent :: Program a -> Division -> Division
makeCongruent p = fixed (execState $ checkProg p)
  where 
    fixed f d' | d' == f d' = d'
               | otherwise = fixed f $ f d'

checkProg :: Program a -> ST ()
checkProg = mapM_ checkBlock

checkBlock :: Block a -> ST ()
checkBlock b = 
  do checkFrom $ from b
     mapM_ checkStep $ body b
     checkJump $ jump b

checkFrom :: IfFrom a -> ST ()
checkFrom (FromCond e _ _ ) = do _ <- checkExpr e; return ()
checkFrom _ = return ()

checkJump :: Jump a -> ST ()
checkJump (If e _ _) = do _ <- checkExpr e; return ()
checkJump _ = return ()

checkStep :: Step -> ST ()
checkStep (UpdateA n e1 _ e2) = 
  do b1 <- checkExpr e1; b2 <- checkExpr e2
     when (b1 || b2) $ setDyn n
checkStep (UpdateV n _ e) = 
  do b <- checkExpr e
     when b $ setDyn n
checkStep (Push n a) = 
  do b1 <- isDyn n; b2 <- isDyn a
     when (b1 || b2) $ setDyns [a,n]
checkStep (Pop n a) = 
  do b1 <- isDyn n; b2 <- isDyn a; 
     when (b1 || b2) $ setDyns [a,n]
checkStep Skip = return ()

checkExpr :: Expr -> ST Bool
checkExpr (Const _)    = return False
checkExpr (Var n)      = isDyn n
checkExpr (Arr n e)    = 
  do b1 <- checkExpr e; b2 <- isDyn n
     when b1 $ setDyn n
     return (b1 || b2)
checkExpr (Op _ e1 e2) = 
  do b1 <- checkExpr e1; b2 <- checkExpr e2 
     return (b1 || b2)
checkExpr (Top n)      = isDyn n
checkExpr (Empty n)    = isDyn n

setDyn :: Name -> ST ()
setDyn n = do d <- get; put $ setType n Dynamic d

setDyns :: [Name] -> ST ()
setDyns = mapM_ setDyn

isDyn :: Name -> ST Bool
isDyn n = gets $ isType n Dynamic 