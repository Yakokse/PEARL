module RL.Impl.Wellformed where

import Utils.Error

import RL.AST
import RL.Program
import RL.Values
import RL.Variables

import qualified Data.Set as S

wellformedProg :: (Eq a, Show a, Eq b, Show b) => Program a b -> EM ()
wellformedProg (decl, p) =
  do _ <- wellformedDecl decl
     _ <- getEntryBlock p
     _ <- getExitBlock p
     let vars = allVars decl
     mapM_ (wellformedBlock p vars) p

wellformedDecl :: VariableDecl -> EM ()
wellformedDecl decl =
  let intraVar = not (repeatedVars inp || repeatedVars out || repeatedVars tmp)
      interVar = notTemp inp && notTemp out
  in if intraVar && interVar
    then return ()
    else Left "Malformed declaration"
  where
    inp = input decl
    out = output decl
    tmp = temp decl
    repeatedVars vs = length vs /= S.size (S.fromList vs)
    notTemp = all (`notElem` tmp)

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
wellformedJump _ (Exit _) = return ()

wellformedFrom :: [Name] -> ComeFrom a b -> EM ()
wellformedFrom _  (From _) = return ()
wellformedFrom ns (Fi e _ _) =
  wellformedExp ns e
wellformedFrom _  (Entry _) = return ()

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
