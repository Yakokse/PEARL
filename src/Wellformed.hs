module Wellformed where

import AST
import Utils
import Values

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