module Wellformed where

import AST
import Utils
import Values
import Division

wellformedProg :: Eq a => (a -> String) -> Program a -> EM ()
wellformedProg format p = 
  do _ <- getEntryBlock p
     _ <- getExitBlock p
     mapM_ (wellformedBlock format p) p

wellformedBlock :: Eq a => (a -> String) -> Program a -> Block a -> EM ()
wellformedBlock format p b = 
  do mapM_ checkFrom $ jumpLabels b
     mapM_ checkGoto $ fromLabels b
  where 
    checkFrom l = do
      b' <- getBlockErr format p l
      if name b `elem` fromLabels b' 
        then return ()
        else Left $ format (name b) ++ " not mentioned in " ++ format l
    checkGoto l = do
      b' <- getBlockErr format p l
      if name b `elem` jumpLabels b' 
        then return ()
        else Left $ format (name b) ++ " not mentioned in " ++ format l

wellformedProg' :: Division -> Program' a -> EM ()
wellformedProg' d = mapM_ wellformedBlock'
  where 
    wellformedBlock' b = do
      wellformedFrom' d $ from' b
      mapM_ (wellformedStep' d) $ body' b
      wellformedJump' d $ jump' b

wellformedFrom' :: Division -> IfFrom' a -> EM ()
wellformedFrom' _ Entry'         = return ()
wellformedFrom' _ (From' _ )     = return ()
wellformedFrom' d (FromCond' l e _ _) =
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
  do ml' <- wellformedPat d (QPair p1 p2)
     case ml' of
      Just l' | l == l' -> return ()
      Nothing -> return ()
      _ -> Left "Pattern mismatch."

-- Nothing represents either type, as constants are nondeterministic o.w.
wellformedPat :: Division -> Pattern -> EM (Maybe Level)
wellformedPat _ (QConst _) = return Nothing
wellformedPat d (QVar n) = return . Just $ getType n d
wellformedPat d (QPair q1 q2) = 
  do ml1 <- wellformedPat d q1
     ml2 <- wellformedPat d q2
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
     if l == Static
      then return Dynamic
      else Left "Lift mismatch"
