module Interpretation.Impl.Interpret where


import Utils.Error
import Utils.Maps
import Utils.PrettyPrint

import RL.AST
import RL.Operators
import RL.Values
import RL.Program

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
prettyStats stats@Stats{steps = s, jumps = j} =
  "Total Steps: " ++ show s
  ++ ", Total Jumps: " ++ show j
  ++ ", Combined Total: " ++ show (totalSteps stats)

prettyStats2 :: Stats -> Stats -> String
prettyStats2 stats1@Stats{steps = s1, jumps = j1} stats2@Stats{steps = s2, jumps = j2} =
  "Total Steps: " ++ show s1 ++ brace (show s2)
      ++ ", Total Jumps: " ++ show j1 ++ brace (show j2)
      ++ ", Combined Total: " ++ show (totalSteps stats1) ++ brace (show $ totalSteps stats2)
  where brace s = " (" ++ s ++ ")"

totalSteps :: Stats -> Int
totalSteps Stats{steps = s, jumps = j} = s + j * 2

-- Base stats for collection
initStats :: Stats
initStats = Stats 0 0 0

-- Interpret a program with a given (verifiable wellformed) input
-- output: program output and statistics
runProgram :: (Eq a, Show a) => Program a () -> Value -> LEM (Value, Stats)
runProgram prog inpValue =
  do  main <- raise $ getMainProcedure prog
      let res = evalProgram prog inpValue main
      S.runStateT res initStats

-- Interpret a program with a (possibly mallformed) input
-- Non-input values in a store are ignored
-- output: program output and statistics
runProgram' :: (Eq a, Show a) => Program a () -> Store -> LEM (Store, Stats)
runProgram' = undefined --TODO: fix when adding PE support for procedures
-- runProgram' (decl, prog) store =
--   do  entry <- raise $ getEntry prog
--       let res = evalBlocks prog (output decl) runStore entry Nothing
--       S.runStateT res initStats
--   where
--     nilStore = fromList . map (\n -> (n, Nil)) $ nonInput decl
--     runStore = combine nilStore store

-- interpret program till exit
-- output: the output value
evalProgram :: (Eq a, Show a) =>
  [Procedure a ()] -> Value -> Procedure a () -> SLEM Value
evalProgram _prog value main = evalProcedure main emptyMap value

evalProcedure :: (Eq a, Show a) =>
  Procedure a () -> Store -> Value -> SLEM Value
evalProcedure procedure store callValue =
  do entryPattern <- S.lift . raise $ getEntryPattern procedure
     procedureStore <- S.lift . raise $ deconstruct store callValue entryPattern
     exitPattern <- S.lift . raise $ getExitPattern procedure
     entry <- S.lift . raise $ getEntry (pbody procedure)
     outputStore <- evalBlocks (pbody procedure) procedureStore entry Nothing
     S.lift . raise $ createExitValue outputStore exitPattern

createExitValue :: Store -> Pattern -> EM Value
createExitValue outputStore exitPattern =
  do (s,v) <- construct outputStore exitPattern
     if isEmpty s || Utils.Maps.all (Nil==) s then Right v else Left "Non-Nil non-output value at procedure exit."


evalBlocks :: (Eq a, Show a) =>
  [Block a ()] ->  Store -> (a, ()) -> Maybe (a, ()) -> SLEM Store
evalBlocks blocks store l origin =
  do block <- S.lift . raise $ getBlockErr blocks l
     (label', store') <- evalBlock store block origin
     case label' of
       Nothing -> return store'
       Just l'  -> evalBlocks blocks store' (l', ()) (Just l)

-- interpret a given block
evalBlock :: (Eq a, Show a) => Store -> Block a () -> Maybe (a, ()) -> SLEM (Maybe a, Store)
evalBlock s b l =
  do S.lift . logM $  show (label b) ++ prettyStore s -- TODO: improve
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
evalFrom _ (Entry _p ()) Nothing = return ()
evalFrom _ _ _ = lift' $ Left "Unexpected jump to entry, or wrong start"

-- interpret a jump statement
-- outputs label of next block
evalJump :: Store -> Jump a () -> SLEM (Maybe a)
evalJump _ (Goto (l, ())) = incJump >> return (Just l)
evalJump s (If e (l1, ()) (l2, ())) = incJump >>
  do v <- lift' $ evalExpr s e
     return . Just $
      if truthy v then l1 else l2
evalJump _ (Exit _p ()) = return Nothing

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
     lift' $ matchPattern s q1 q2
evalStep s (Update n op e) =
  do v1 <- lift' $ find n s
     v2 <- lift' $ evalExpr (s `without` n) e
     v3 <- lift' $ calcR op v1 v2
     return $ set n v3 s

matchPattern :: Store -> Pattern -> Pattern -> EM Store
matchPattern s q1 q2 =
    do (s1, v) <- construct s q2
       deconstruct s1 v q1

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
    _ -> return Nil -- Initialize new variables to Nil

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
