module Impl.Execute where

import AST
import Operators
import Values
import Utils
import PrettyPrint
import Impl.Maps

import qualified Control.Monad.State as S

type SLEM = S.StateT Stats LEM

-- Monad utility function
lift' :: EM a -> SLEM a
lift' = S.lift . raise

data Stats = Stats
  { steps :: Int
  , jumps :: Int
  , assertions :: Int
  } deriving Show

prettyStats :: Stats -> String
prettyStats Stats{steps = s, jumps = j} =
  "Total Steps: " ++ show s ++ ", Total Jumps: " ++ show j ++ ", Combined Total: " ++ show total
    where total = s + j * 2

-- Base stats for collection
initStats :: Stats
initStats = Stats 0 0 0

-- Interpret a program with a given (verifiable wellformed) input
-- output: program output and statistics
runProgram :: (Eq a, Show a) => Program a () -> Store -> LEM (Store, Stats)
runProgram (decl, prog) inpstore =
  do  entry <- raise $ getEntry prog
      store <- raise $ createStore decl inpstore
      let res = evalBlocks prog (output decl) store entry Nothing
      S.runStateT res initStats

-- Interpret a program with a (possibly mallformed) input
-- Non-input values in a store are ignored
-- output: program output and statistics
runProgram' :: (Eq a, Show a) => Program a () -> Store -> LEM (Store, Stats)
runProgram' (decl, prog) store =
  do  entry <- raise $ getEntry prog
      let res = evalBlocks prog (output decl) runStore entry Nothing
      S.runStateT res initStats
  where
    nilStore = fromList . map (\n -> (n, Nil)) $ nonInput decl
    runStore = combine nilStore store

-- Create a proper store given an input store
-- verifies that input store is wellformed
createStore :: VariableDecl -> Store -> EM Store
createStore decl store =
  let anyTemp = any (\n -> n `elem` temp decl) (keys store)
      anyOut = any (\n -> n `elem` output decl
                       && n `notElem` output decl) (keys store)
      allPresent = all (`elem` keys store) (input decl)
  in if anyTemp || anyOut || not allPresent
  then Left "Invalid input store"
  else
    let nilStore = fromList . map (\n -> (n, Nil)) $ nonInput decl
    in return $ combine store nilStore

-- interpret program till exit
-- output: the output store
evalBlocks :: (Eq a, Show a) =>
  [Block a ()] -> [Name] -> Store -> (a, ()) -> Maybe (a, ()) -> SLEM Store
evalBlocks prog outputs store l origin =
  do block <- S.lift . raise $ getBlockErr prog l
     (label', store') <- evalBlock store block origin
     case label' of
       Nothing -> return $ store' `onlyIn` outputs
       Just l'  -> evalBlocks prog outputs store' (l', ()) (Just l)

-- interpret a given block
evalBlock :: (Eq a, Show a) => Store -> Block a () -> Maybe (a, ()) -> SLEM (Maybe a, Store)
evalBlock s b l =
  do S.lift . logM $  show (fst $ name b) ++ prettyStore s -- TODO: improve
     evalFrom s (from b) l
     s' <- evalSteps s (body b)
     l' <- evalJump s' (jump b)
     return (l', s')

-- interpret a come-from statement
-- error if control-flow violates backwards determinism
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

-- interpret a jump statement
-- outputs label of next block
evalJump :: Store -> Jump a () -> SLEM (Maybe a)
evalJump _ (Goto (l, ())) = incJump >> return (Just l)
evalJump s (If e (l1, ()) (l2, ())) = incJump >>
  do v <- lift' $ evalExpr s e
     return . Just $
      if truthy v then l1 else l2
evalJump _ (Exit ()) = return Nothing

-- interpret multiple steps
evalSteps :: Store -> [Step] -> SLEM Store
evalSteps = S.foldM (\store step -> incStep >> evalStep store step)

-- interpret a given step
evalStep :: Store -> Step -> SLEM Store
evalStep s Skip = return s
evalStep s (Assert e) =
  do incAssert
     v <- lift'$ evalExpr s e
     if truthy v then return s
     else lift' $ Left  $ "failed assertion: " ++ show e
evalStep s (Replacement q1 q2) =
  do (s1, v) <- lift' $ construct s q2
     lift' $ deconstruct s1 v q1
evalStep s (Update n op e) =
  do v1 <- lift' $ find n s
     v2 <- lift' $ evalExpr (s `without` n) e
     v3 <- lift' $ calcR op v1 v2
     return $ set n v3 s

-- construct an intermediate value and store for a replacement
construct :: Store -> Pattern -> EM (Store, Value)
construct store (QConst v) = return (store,v)
construct store (QVar n) =
  do v <- find n store
     let store' = set n Nil store
     return (store', v)
construct store (QPair q1' q2') =
  do (store', v)   <- construct store q1'
     (store'', v') <- construct store' q2'
     return (store'', Pair v v')

-- deconstruct intermediate value into new store
-- errors if cannot match
deconstruct :: Store -> Value -> Pattern -> EM Store
deconstruct store v (QConst v') =
  if v == v'
    then return store
    else Left "Non-matching constants in replacement."
deconstruct store v (QVar n) =
  do v' <- find n store
     if v' == Nil
      then return $ set n v store
      else Left "Non-nill variable in replacement."
deconstruct store (Pair v1 v2) (QPair q1' q2') =
  do store' <- deconstruct store v1 q1'
     deconstruct store' v2 q2'
deconstruct _ _ (QPair _ _) = Left "Scalar value with cons pattern in replacement."

-- evaluate an expression
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

find :: Name -> Store -> EM Value
find n s =
  case lookupM n s of
    Just v -> return v
    _ -> Left $ "Variable \"" ++ n ++ "\" not found during lookup"

-- helper functions for statistics
incAssert :: SLEM ()
incAssert =
  do stats <- S.get
     S.put (stats{assertions = assertions stats + 1})

incJump :: SLEM ()
incJump =
  do stats <- S.get
     S.put (stats{jumps = jumps stats + 1})

incStep :: SLEM ()
incStep =
  do stats <- S.get
     S.put (stats{steps = steps stats + 1})
