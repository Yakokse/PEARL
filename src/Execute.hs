module Execute where

import AST
import Operators
import Values
import Control.Monad.State

data Stats = Stats
  { steps :: Int
  , jumps :: Int
  , assertions :: Int
  }

type SEM = StateT Stats EM

initStats :: Stats
initStats = Stats 0 0 0

evalBlock :: Eq a => Store -> Block a -> a -> SEM (a, Store)
evalBlock = undefined

evalFrom :: Eq a => Store -> IfFrom a -> a -> SEM ()
evalFrom = undefined

evalJump :: Store -> Jump a -> SEM a
evalJump = undefined

evalStep :: Store -> Step -> SEM Store
evalStep s Skip = return s
evalStep s (Assert e) =
  do incAssert
     v <- lift $ evalExpr s e
     if truthy v then return s
     else lift $ Left "failed assertion"
evalStep s (Replacement q1 q2) =
  do (s1, v) <- lift $ deconstruct s q2
     lift $ construct s1 v q1
evalStep s (Update n op e) =
  do v1 <- lift $ find n s
     v2 <- lift $ evalExpr (s `without` n) e
     v3 <- lift $ calcR op v1 v2
     return $ update n v3 s

deconstruct :: Store -> Pattern -> EM (Store, Value)
deconstruct store (QConst v) = return (store,v)
deconstruct store (QVar n) = 
  do v <- find n store
     let store' = update n Nil store
     return (store', v)
deconstruct store (QPair q1' q2') =
  do (store', v)   <- deconstruct store q1'
     (store'', v') <- deconstruct store' q2'
     return (store'', Pair v v')

construct :: Store -> Value -> Pattern -> EM Store
construct store v (QConst v') =
  if v == v' 
    then return store 
    else Left "Non-matching constants in replacement."
construct store v (QVar n) =
  do v' <- find n store
     if v' == Nil 
      then return $ update n v store 
      else Left "Non-nill variable in replacement."
construct store (Pair v1 v2) (QPair q1' q2') =
  do store' <- construct store v1 q1'
     construct store' v2 q2'
construct _ _ (QPair _ _) = Left "Scalar value with cons pattern in replacement."     

evalExpr :: Store -> Expr -> EM Value
evalExpr _ (Const v) = return v
evalExpr s (Var n) = find n s
evalExpr s (Op op e1 e2) =
  do v1 <- evalExpr s e1
     v2 <- evalExpr s e2
     calc op v1 v2
evalExpr s (UOp op e) =
  do v <- evalExpr s e
     calcU op v

incAssert :: SEM ()
incAssert =
  do stats <- get
     put (stats{assertions = assertions stats + 1})
