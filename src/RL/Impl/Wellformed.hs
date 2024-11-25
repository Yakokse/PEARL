module RL.Impl.Wellformed where

import Utils.Error

import RL.AST
import RL.Program
import RL.Values
import RL.Variables

import qualified Data.Set as S
import RL.AST (Procedure)

wellformedProg :: (Eq a, Show a, Eq b, Show b) => Program a b -> EM ()
wellformedProg p =
  do
    entryProcedure <- getEntryProcedure p
    _ <- getEntryBlock (pbody entryProcedure)
    _ <- getExitBlock (pbody entryProcedure)
    mapM_ (welformedProcedures p) p

welformedProcedures :: (Eq a, Show a, Eq b, Show b) => [Procedure a b] -> Procedure a b -> EM ()
welformedProcedures = undefined -- TODO: implement

wellformedBlock :: (Eq a, Show a, Eq b, Show b) => [Block a b] -> [Name] -> Block a b -> EM ()
wellformedBlock p ns b =
  do mapM_ checkFrom $ jumpLabels $ jump b
     mapM_ checkGoto $ fromLabels $ from b
     mapM_ (wellformedStep ns) $ body b
     wellformedJump ns $ jump b
     wellformedFrom ns $ from b
  where
    checkFrom l = do
      b' <- getBlockErr p l
      if name b `elem` fromLabels (from b')
        then return ()
        else Left $ show (name b) ++ " not mentioned in come-from of " ++ show l
    checkGoto l = do
      b' <- getBlockErr p l
      if name b `elem` jumpLabels (jump b')
        then return ()
        else Left $ show (name b) ++ " not mentioned in jump of " ++ show l ++
                "\n\nInstead:" ++ show (jump b')

wellformedJump :: [Name] -> Jump a b -> EM ()
wellformedJump _ (Goto _) = return ()
wellformedJump ns (If e _ _) =
  wellformedExp ns e
wellformedJump ns (Exit p _) = wellformedPat ns p

wellformedFrom :: [Name] -> ComeFrom a b -> EM ()
wellformedFrom _  (From _) = return ()
wellformedFrom ns (Fi e _ _) =
  wellformedExp ns e
wellformedFrom ns  (Entry p _) = wellformedPat ns p

wellformedStep :: [Name] -> Step -> EM ()
wellformedStep _ Skip = return ()
wellformedStep ns (Assert e) =
  wellformedExp ns e
wellformedStep ns (Replacement q1 q2) =
  wellformedPat ns q1 >> wellformedPat ns q2
wellformedStep ns (Update n _ e) =
  do isDefined n ns
     wellformedExp (filter (/= n) ns) e

wellformedPat :: [Name] -> Pattern -> EM ()
wellformedPat _ (QConst _) = return ()
wellformedPat ns (QVar n) = isDefined n ns
wellformedPat ns (QPair q1 q2) =
  if all (`notElem` getVarsPat q2) $ getVarsPat q1
  then do wellformedPat ns q1
          wellformedPat ns q2
  else Left "Non-linear pattern"

wellformedExp :: [Name] -> Expr -> EM ()
wellformedExp _ (Const _) = return ()
wellformedExp ns (Var n) =
  isDefined n ns
wellformedExp ns (Op _ e1 e2) =
  do wellformedExp ns e1
     wellformedExp ns e2
wellformedExp ns (UOp _ e) =
  wellformedExp ns e

isDefined :: Name -> [Name] -> EM ()
isDefined n ns =
  if n `elem` ns
    then return ()
    else Left $ "Variable \"" ++ n ++ "\" not defined (or not available here)"
