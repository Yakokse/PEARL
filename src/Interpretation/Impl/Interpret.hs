module Interpretation.Impl.Interpret where


import Utils.Error
import Utils.Maps
import Utils.PrettyPrint

import RL.AST
import RL.Operators
import RL.Values
import RL.Program

import qualified Control.Monad.State as S
import qualified Data.Foldable
import Inversion.Impl.Inverter

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
  let main = getMainProcedure prog
      res = evalProgram prog inpValue main
  in S.runStateT res initStats

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
  Program a () -> Value -> Procedure a () -> SLEM Value
evalProgram prog value main = evalProcedure prog main value

evalProcedure :: (Eq a, Show a) =>
  Program a () -> Procedure a () -> Value -> SLEM Value
evalProcedure prog procedure callValue =
  do entryPattern <- lift' $ getEntryPattern procedure
     procedureStore <- deconstruct prog emptyStore callValue entryPattern
     exitPattern <- lift' $ getExitPattern procedure
     entry <- lift' $ getEntry (pbody procedure)
     outputStore <- evalBlocks prog (pbody procedure) procedureStore entry Nothing
     createExitValue prog outputStore exitPattern

createExitValue :: (Eq a, Show a) => Program a () -> Store -> Pattern -> SLEM Value
createExitValue prog outputStore exitPattern =
  do (s,v) <- construct prog outputStore exitPattern
     lift' $ if Utils.Maps.all (Nil==) s
             then Right v
             else Left "Non-Nil non-output variable at procedure exit."


evalBlocks :: (Eq a, Show a) =>
  Program a () -> [Block a ()] ->  Store -> (a, ()) -> Maybe (a, ()) -> SLEM Store
evalBlocks prog blocks store l origin =
  do block <- lift' $ getBlockErr blocks l
     (label', store') <- evalBlock prog store block origin
     case label' of
       Nothing -> return store'
       Just l'  -> evalBlocks prog blocks store' (l', ()) (Just l)

-- interpret a given block
evalBlock :: (Eq a, Show a) => Program a () -> Store -> Block a () -> Maybe (a, ()) -> SLEM (Maybe a, Store)
evalBlock p s b l =
  do S.lift . logM $  show (label b) ++ prettyStore s -- TODO: improve
     evalFrom s (from b) l
     s' <- evalSteps p s (body b)
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
evalSteps :: (Eq a, Show a) => Program a () -> Store -> [Step] -> SLEM Store
evalSteps program = S.foldM (\store step -> incStep >> evalStep program store step)

-- interpret a given step
evalStep :: (Eq a, Show a) => Program a () -> Store -> Step -> SLEM Store
evalStep _ s Skip = return s
evalStep _ s (Assert e) =
  do incAssert
     v <- lift'$ evalExpr s e
     if truthy v then return s
     else lift' $ Left  $ "failed assertion: " ++ show e
evalStep p s (Replacement q1 q2) = matchPattern p s q1 q2
evalStep _ s (Update n op e) =
  let v1 = find n s
  in do
     v2 <- lift' $ evalExpr (s `without` n) e
     v3 <- lift' $ calcR op v1 v2
     return $ set n v3 s

matchPattern :: (Eq a, Show a) => Program a () -> Store -> Pattern -> Pattern -> SLEM Store
matchPattern prog s q1 q2 =
    do (s1, v) <- construct prog s q2
       deconstruct prog s1 v q1

-- call a procedure
call :: (Eq a, Show a) => Program a () -> ProcedureName -> Value -> SLEM Value
call prog procName val =
  let procedure = Data.Foldable.find (\p -> pname p == procName) prog
  in case procedure of
     (Just procedure') -> evalProcedure prog procedure' val
     Nothing -> lift' $ Left ("Undefined procedure: " ++ procName)

-- uncall a procedure (reverse evaluation)
uncall :: (Eq a, Show a) => Program a () -> ProcedureName -> Value -> SLEM Value
uncall prog procName val =
  let procedure = Data.Foldable.find (\p -> pname p == procName) prog
  in case procedure of
     (Just procedure') -> evalProcedure prog (invertProc procedure') val
     Nothing -> lift' $ Left ("Undefined procedure: " ++ procName)

-- construct an intermediate value and store for a replacement
construct :: (Eq a, Show a) => Program a () -> Store -> Pattern -> SLEM (Store, Value)
construct _ store (QConst v) = return (store,v)
construct _ store (QVar n) =
  let v = find n store
      store' = set n Nil store
  in return (store', v)
construct prog store (QPair q1' q2') =
  do (store', v)   <- construct prog store q1'
     (store'', v') <- construct prog store' q2'
     return (store'', Pair v v')
construct prog store (QCall procName pattern) =
  do
    (store',val) <- construct prog store pattern
    out <- call prog procName val
    lift' $ Right (store',out)
construct prog store (QUncall procName pattern) =
  do
    (store',val) <- construct prog store pattern
    out <- uncall prog procName val
    lift' $ Right (store',out)

-- deconstruct intermediate value into new store
-- errors if cannot match
deconstruct :: (Eq a, Show a) => Program a () -> Store -> Value -> Pattern -> SLEM Store
deconstruct _ store v (QConst v') =
  if v == v'
    then return store
    else lift' $ Left "Non-matching constants in replacement."
deconstruct _ store v (QVar n) =
  let v' = find n store
  in if v' == Nil
     then return $ set n v store
     else lift' $ Left "Non-nill variable in replacement."
deconstruct prog store (Pair v1 v2) (QPair q1' q2') =
  do store' <- deconstruct prog store v1 q1'
     deconstruct prog store' v2 q2'
deconstruct _ _ _ (QPair _ _) = lift' $ Left "Scalar value with cons pattern in replacement."
deconstruct prog store v (QCall procName pattern) =
  do
    out <- uncall prog procName v
    deconstruct prog store out pattern
deconstruct prog store v (QUncall procName pattern) =
  do
    out <- call prog procName v
    deconstruct prog store out pattern

-- evaluate an expression
evalExpr :: Store -> Expr -> EM Value
evalExpr _ (Const v) = return v
evalExpr s (Var n) = return (find n s)
evalExpr s (Op op e1 e2) =
  do v1 <- evalExpr s e1
     v2 <- evalExpr s e2
     calc op v1 v2
evalExpr s (UOp op e) =
  do v <- evalExpr s e
     calcU op v

find :: Name -> Store -> Value
find n s =
  case lookupM n s of
    Just v -> v
    _ -> Nil -- Initialize new variables to Nil

emptyStore :: Store
emptyStore = emptyMap

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
