module Wellformed where

import AST
import AST2
import Utils
import Values
import Division
import Data.Set ( fromList, size )

wellformedProg :: (Eq a, Show a) => Program a () -> EM ()
wellformedProg (decl, p) = 
  do _ <- wellformedDecl decl
     _ <- getEntryBlock p
     _ <- getExitBlock p
     let allVars = getVarsDecl decl
     mapM_ (wellformedBlock (decl, p) allVars) p

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
    repeatedVars vs = length vs /= size (fromList vs)
    notTemp = all (`notElem` tmp)
    

wellformedBlock :: (Eq a, Show a) => Program a () -> [Name] -> Block a () -> EM ()
wellformedBlock p ns b = 
  do mapM_ checkFrom $ jumpLabels $ jump b
     mapM_ checkGoto $ fromLabels $ from b
     mapM_ (wellformedStep ns) $ body b
     wellformedJump ns $ jump b
     wellformedFrom ns $ from b
  where 
    checkFrom l = do
      b' <- getBlockErr (snd p) l
      if name b `elem` fromLabels (from b') 
        then return ()
        else Left $ show (name b) ++ " not mentioned in " ++ show l
    checkGoto l = do
      b' <- getBlockErr (snd p) l
      if name b `elem` jumpLabels (jump b') 
        then return ()
        else Left $ show (name b) ++ " not mentioned in " ++ show l

wellformedJump :: [Name] -> Jump a () -> EM ()
wellformedJump _ (Goto _) = return ()
wellformedJump ns (If e _ _) = 
  wellformedExp ns e
wellformedJump _ (Exit _) = return ()

wellformedFrom :: [Name] -> ComeFrom a () -> EM ()
wellformedFrom _  (From _) = return ()
wellformedFrom ns (Fi e _ _) = 
  wellformedExp ns e
wellformedFrom _  (Entry _) = return ()

wellformedStep :: [Name] -> Step -> EM ()
wellformedStep _ Skip = return ()
wellformedStep ns (Assert e) = 
  wellformedExp ns e
wellformedStep ns (Replacement q1 q2) =
  wellformedPat ns (QPair q1 q2)
wellformedStep ns (Update n _ e) =
  do isDefined n ns
     wellformedExp (filter (/= n) ns) e

wellformedPat :: [Name] -> Pattern -> EM ()
wellformedPat _ (QConst _) = return ()
wellformedPat ns (QVar n) = isDefined n ns
wellformedPat ns (QPair q1 q2) =
  do wellformedPat ns q1
     wellformedPat ns q2

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

wellformedProg' :: Division -> Program' a -> EM ()
wellformedProg' d = mapM_ wellformedBlock'
  where 
    wellformedBlock' b = do
      wellformedFrom' d $ from' b
      mapM_ (wellformedStep' d) $ body' b
      wellformedJump' d $ jump' b

wellformedFrom' :: Division -> ComeFrom' a -> EM ()
wellformedFrom' _ Entry'         = return ()
wellformedFrom' _ (From' _ )     = return ()
wellformedFrom' d (Fi' l e _ _) =
  do l' <- wellformedExp' d e
     if l == l' then return ()
     else Left "From mismatch."

wellformedStep' :: Division -> Step' -> EM ()
wellformedStep' _ (Skip' _) = return ()
wellformedStep' d (Assert' l e) = 
  do l' <- wellformedExp' d e
     if l == l' then return ()
     else Left "Assert mismatch" 
wellformedStep' d (Update' l n _ e) =
  do l' <- wellformedExp' d e
     if l == l' && l == getType n d
      then return ()
      else Left "Update mismatch."
wellformedStep' d (Replacement' l p1 p2) =
  do ml' <- wellformedPat' d (QPair p1 p2)
     case ml' of
      Just l' | l == l' -> return ()
      Nothing -> return ()
      _ -> Left "Pattern mismatch."

-- Nothing represents either type, as constants are nondeterministic o.w.
wellformedPat' :: Division -> Pattern -> EM (Maybe Level)
wellformedPat' _ (QConst _) = return Nothing
wellformedPat' d (QVar n) = return . Just $ getType n d
wellformedPat' d (QPair q1 q2) = 
  do ml1 <- wellformedPat' d q1
     ml2 <- wellformedPat' d q2
     case ml1 of
      Nothing -> return ml2
      Just t1 -> 
        case ml2 of 
          Just t2 | t1 /= t2 -> Left "Pattern mismatch"
          _ -> return $ Just t1


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
