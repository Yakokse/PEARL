module RL.Impl.Wellformed where

import Utils.Error

import RL.AST
import RL.Program
import RL.Values
import RL.Variables
import Data.List (nub)
import Control.Monad (void)

wellformedProg :: (Showable a, Showable b) => Program a b -> EM ()
wellformedProg p =
    let pnames = map pname p
    in if nub pnames == pnames
       then mapM_ (wellformedProcedure p) p
       else Left "Duplicate procedure names found."

wellformedProcedure :: (Showable a, Showable b) => Program a b -> Procedure a b -> EM ()
wellformedProcedure p f =
  do _ <- getEntryBlock f
     _ <- getExitBlock f
     let bnames = map name $ pbody f
     if nub bnames == bnames
      then mapM_ (wellformedBlock p f) (pbody f)
      else Left $ "Duplicate block names found in procedure: " ++ pname f

wellformedBlock :: (Showable a, Showable b) => Program a b -> Procedure a b -> Block a b -> EM ()
wellformedBlock p f b =
  do wellformedJump f (name b) $ jump b
     mapM_ (wellformedStep p) $ body b
     wellformedFrom f (name b) $ from b

wellformedJump :: (Showable a, Showable b) => Procedure a b -> (a, b) -> Jump a b -> EM ()
wellformedJump f orig (Goto dest) =
  do b <- getBlockErr f dest
     if orig `notElem` fromLabels (from b)
        then Left $ show (fst orig) ++ " not mentioned in come-from of " ++ show (name b)
        else return ()
wellformedJump f orig (If _ dest1 dest2) | dest1 == dest2 = Left $ "Duplicate labels in jump of " ++ show (fst orig)
                                         | otherwise =
  do b1 <- getBlockErr f dest1
     b2 <- getBlockErr f dest2
     if any (\b -> orig `notElem` fromLabels (from b)) [b1, b2]
        then Left $ "One of destinations does not mention " ++ show (fst orig) ++ " in come-from."
        else return ()
wellformedJump _ _ (Exit p _) = void $ wellformedSubPat p

wellformedFrom :: (Showable a, Showable b) => Procedure a b -> (a, b) -> ComeFrom a b -> EM ()
wellformedFrom f dest (From orig) =
  do b <- getBlockErr f orig
     if dest `notElem` jumpLabels (jump b)
        then Left $ show (fst orig) ++ " not mentioned in jump of " ++ show (name b)
        else return ()
wellformedFrom f dest (Fi _ orig1 orig2) | orig1 == orig2 = Left $ "Duplicate labels in come-from of " ++ show (fst dest)
                                         | otherwise =
  do b1 <- getBlockErr f orig1
     b2 <- getBlockErr f orig2
     if any (\b -> dest `notElem` jumpLabels (jump b)) [b1, b2]
        then Left $ "One of origins does not mention " ++ show (fst dest) ++ " in jump."
        else return ()
wellformedFrom _ _ (Entry p _) = void $ wellformedSubPat p

wellformedStep :: Program a b -> Step -> EM ()
wellformedStep _ Skip = return ()
wellformedStep _ (Assert _) = return ()
wellformedStep p (Replacement q1 q2) =
  wellformedPat p q1 >> wellformedPat p q2 >> return ()
wellformedStep _ (Update n _ e) | n `elem` getVarsExp e = Left $ "Variable " ++ show n ++ "occurs on RHS of update of itself."
                                | otherwise             = return ()

wellformedPat :: Program a b -> Pattern -> EM [Name]
wellformedPat _ (QConst _) = return []
wellformedPat _ (QVar n) = return [n]
wellformedPat p (QPair q1 q2) =
  do ns1 <- wellformedPat p q1
     ns2 <- wellformedPat p q2
     if any (`elem` ns2) ns1
      then Left "Non-linear pattern"
      else return $ ns1 ++ ns2
wellformedPat p (QCall f q) =
  if f `notElem` map pname p
    then Left $ "Procedure " ++ show f ++ " is referenced, but does not exist."
    else wellformedSubPat q
wellformedPat p (QUncall f q) =
  if f `notElem` map pname p
    then Left $ "Procedure " ++ show f ++ " is referenced, but does not exist."
    else wellformedSubPat q

wellformedSubPat :: Pattern -> EM [Name]
wellformedSubPat (QConst _) = return []
wellformedSubPat (QVar n) = return [n]
wellformedSubPat (QPair q1 q2) =
  do ns1 <- wellformedSubPat q1
     ns2 <- wellformedSubPat q2
     if any (`elem` ns2) ns1
      then Left "Non-linear pattern"
      else return $ ns1 ++ ns2
wellformedSubPat (QCall f _) = Left $ "Unexpected call to " ++ show f ++ "when no calls were expected in pattern."
wellformedSubPat (QUncall f _) = Left $ "Unexpected call to " ++ show f ++ "when no calls were expected in pattern."

isDefined :: Name -> [Name] -> EM ()
isDefined n ns =
  if n `elem` ns
    then return ()
    else Left $ "Variable \"" ++ n ++ "\" not defined (or not available here)"
