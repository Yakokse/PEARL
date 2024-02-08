module Execute where

import AST
import Operators
import Values
import Utils
import PrettyPrint

import Control.Monad.State

data Stats = Stats
  { steps :: Int
  , jumps :: Int
  , assertions :: Int
  } deriving Show

type SLEM = StateT Stats LEM

lift' :: EM a -> SLEM a
lift' = lift . raise 

initStats :: Stats
initStats = Stats 0 0 0

runProgram :: (Eq a, Show a) => Program a () -> Store -> LEM (Store, Stats)
runProgram (decl, prog) inpstore =
  do  entry <- raise $ getEntry prog
      store <- raise $ createStore decl inpstore
      let res = evalBlocks prog store entry Nothing
      runStateT res initStats

createStore :: VariableDecl -> Store -> EM Store
createStore decl store =
  let anyTemp = any (\n -> n `elem` temp decl) (vars store)
      anyOut = any (\n -> n `elem` output decl
                       && n `notElem` output decl) (vars store)
      allPresent = all (`elem` vars store) (input decl)
  in if anyTemp || anyOut || not allPresent
  then Left "Invalid input store"
  else 
    let nilStore = makeStore . map (\n -> (n, Static Nil)) $ nonInput decl
    in return $ nilStore `updateWithStore` store
    
evalBlocks :: (Eq a, Show a) => 
  [Block a ()] -> Store -> (a, ()) -> Maybe (a, ()) -> SLEM Store
evalBlocks prog store l origin =
  do block <- lift . raise $ getBlockErr prog l
     (label', store') <- evalBlock store block origin
     case label' of
       Nothing -> return store'
       Just l'  -> evalBlocks prog store' (l', ()) (Just l)

evalBlock :: (Eq a, Show a) => Store -> Block a () -> Maybe (a, ()) -> SLEM (Maybe a, Store)
evalBlock s b l = 
  do lift . logM $ prettyAnn show (name b, s)
     evalFrom s (from b) l
     s' <- evalSteps s (body b)
     l' <- evalJump s' (jump b)
     return (l', s')

evalFrom :: Eq a => Store -> ComeFrom a ()-> Maybe (a, ()) -> SLEM ()
evalFrom _ (From (l, ())) (Just (l', ())) =
  if l == l' then return ()
  else lift' $ Left "Unconditional from failed"
evalFrom s (Fi e (l1, ()) (l2, ())) (Just (l', ())) =
  do v <- lift' $ evalExpr s e
     let l = if truthy v then l1 else l2
     if l == l' then return ()
     else lift' $ Left "Assertion failed in Fi"
evalFrom _ (Entry ()) Nothing = return ()
evalFrom _ _ _ = lift' $ Left "Unexpected jump to entry, or wrong start"

evalJump :: Store -> Jump a () -> SLEM (Maybe a)
evalJump _ (Goto (l, ())) = incJump >> return (Just l)
evalJump s (If e (l1, ()) (l2, ())) = incJump >>
  do v <- lift' $ evalExpr s e
     return . Just $ 
      if truthy v then l1 else l2
evalJump _ (Exit ()) = return Nothing

evalSteps :: Store -> [Step] -> SLEM Store
evalSteps = foldM (\store step -> incStep >> evalStep store step)

evalStep :: Store -> Step -> SLEM Store
evalStep s Skip = return s
evalStep s (Assert e) =
  do incAssert
     v <- lift'$ evalExpr s e
     if truthy v then return s
     else lift' $ Left "failed assertion"
evalStep s (Replacement q1 q2) =
  do (s1, v) <- lift' $ deconstruct s q2
     lift'$ construct s1 v q1
evalStep s (Update n op e) =
  do v1 <- lift' $ find n s
     v2 <- lift' $ evalExpr (s `without` n) e
     v3 <- lift' $ calcR op v1 v2
     return $ update n (Static v3) s

deconstruct :: Store -> Pattern -> EM (Store, Value)
deconstruct store (QConst v) = return (store,v)
deconstruct store (QVar n) = 
  do v <- find n store
     let store' = update n (Static Nil) store
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
      then return $ update n (Static v) store 
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

incAssert :: SLEM ()
incAssert =
  do stats <- get
     put (stats{assertions = assertions stats + 1})

incJump :: SLEM ()
incJump =
  do stats <- get
     put (stats{jumps = jumps stats + 1})

incStep :: SLEM ()
incStep =
  do stats <- get
     put (stats{steps = steps stats + 1})
