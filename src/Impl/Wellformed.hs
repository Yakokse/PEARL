module Impl.Wellformed where

import AST
import AST2
import Utils
import Values
import Division
import qualified Data.Set as S

wellformedProg :: (Eq a, Show a, Eq b, Show b) => Program a b -> EM ()
wellformedProg (decl, p) =
  do _ <- wellformedDecl decl
     _ <- getEntryBlock p
     _ <- getExitBlock p
     let allVars = getVarsDecl decl
     mapM_ (wellformedBlock p allVars) p

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
  if S.fromList (getVarsPat q1) `S.disjoint` S.fromList (getVarsPat q2)
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

wellformedProg' :: Ord a => DivisionPW a -> Program' a -> EM ()
wellformedProg' pwd = mapM_ wellformedBlock'
  where
    wellformedBlock' b = do
      let (d1, d2) = getDivs (name' b) pwd
      wellformedFrom' d1 $ from' b
      mapM_ (wellformedStep' d1 d2) $ body' b
      wellformedJump' d2 $ jump' b

wellformedFrom' :: Division -> ComeFrom' a -> EM ()
wellformedFrom' _ Entry'         = return ()
wellformedFrom' _ (From' _ )     = return ()
wellformedFrom' d (Fi' l e _ _) =
  do l' <- wellformedExp' d e
     if l == l' then return ()
     else Left "From mismatch."

wellformedStep' :: Division -> Division -> Step' -> EM ()
wellformedStep' _ _ (Skip' _) = return ()
wellformedStep' d _ (Assert' l e) =
  do l' <- wellformedExp' d e
     if l == l' then return ()
     else Left "Assert mismatch"
wellformedStep' d _ (Update' l n _ e) =
  do l' <- wellformedExp' d e
     if l == l' && l == getType n d
      then return ()
      else Left "Update mismatch."
wellformedStep' d1 d2 (Replacement' l p1 p2) =
  do l1 <- wellformedPat' d2 p1
     l2 <- wellformedPat' d1 p2
     if l == l1 && l1 == l2
      then return ()
      else Left "Pattern mismatch."
wellformedStep' d1 d2 (Generalize n) =
  if getType n d1 == BTStatic && getType n d2 == BTDynamic
    then return ()
    else Left $ "Unexpected explicator: " ++ n

wellformedPat' :: Division -> Pattern' -> EM Level
wellformedPat' _ (QConst' l _) = return l
wellformedPat' d (QVar' l n) =
  if getType n d == l
    then return l
    else Left $ "Variable mismatch: " ++ n
wellformedPat' d (QPair' l q1 q2) =
  do l1 <- wellformedPat' d q1
     l2 <- wellformedPat' d q2
     if l == BTDynamic || (l1 == l2 && l2 == BTStatic)
      then return l
      else Left "Type mismatch in pair"
wellformedPat' _ _ = return BTDynamic

wellformedJump' :: Division -> Jump' a -> EM ()
wellformedJump' _ Exit'         = return ()
wellformedJump' _ (Goto' _ )    = return ()
wellformedJump' d (If' l e _ _) =
  do l' <- wellformedExp' d e
     if l == l' then return ()
     else Left "Jump mismatch"

wellformedExp' :: Division -> Expr' -> EM Level
wellformedExp' _ (Const' l _) = return l
wellformedExp' d (Var' l n) =
  if getType n d == l
    then return l
    else Left $ "Variable mismatch: " ++ n
wellformedExp' d (Op' l op e1 e2) =
  do l1 <- wellformedExp' d e1
     l2 <- wellformedExp' d e2
     if l == l1 && l == l2
      then return l
      else Left $ "Bin.Op. mismatch: " ++ show op
wellformedExp' d (UOp' l op e) =
  do l' <- wellformedExp' d e
     if l == l'
      then return l
      else Left $ "Un.Op. mismatch: " ++ show op
wellformedExp' d (Lift e) =
  do l <- wellformedExp' d e
     if l == BTStatic
      then return BTDynamic
      else Left "Lift mismatch"
