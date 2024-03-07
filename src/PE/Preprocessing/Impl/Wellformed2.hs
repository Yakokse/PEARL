module PE.Preprocessing.Impl.Wellformed2 where

import Utils.Maps
import Utils.Error

import PE.AST2
import PE.SpecValues
import PE.Preprocessing.Division

wellformedProg' :: Ord a => PWDivision a -> Program' a -> EM ()
wellformedProg' pwd = mapM_ wellformedBlock'
  where
    wellformedBlock' b = do
      let (d1, d2) = get (name' b) pwd
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
     if l == l' && l == get n d
      then return ()
      else Left "Update mismatch."
wellformedStep' d1 d2 (Replacement' l p1 p2) =
  do l1 <- wellformedPat' d2 p1
     l2 <- wellformedPat' d1 p2
     if l == l1 && l1 == l2
      then return ()
      else Left "Pattern mismatch."
wellformedStep' d1 d2 (Generalize n) =
  if get n d1 == BTStatic && get n d2 == BTDynamic
    then return ()
    else Left $ "Unexpected explicator: " ++ n

wellformedPat' :: Division -> Pattern' -> EM Level
wellformedPat' _ (QConst' l _) = return l
wellformedPat' d (QVar' l n) =
  if get n d == l
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
  if get n d == l
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
