module PE.Preprocessing.Division where

import Utils.Maps
import Utils.Error

import RL.AST
import RL.Values
import RL.Variables
import RL.Program

import PE.SpecValues

type Division = Map Name Level
type PWDivision l = Map l (Division, Division)

-- check if variable is given type in division
isType :: Name -> Level -> Division -> Bool
isType n t d = get n d == t

boundedBy :: Name -> Level -> Division -> Division
boundedBy = update lub

-- make a division for all variables in a declaration
-- sets all variables to static
makeStaticDiv :: VariableDecl -> Division
makeStaticDiv decl =
  let pairs = map (\n -> (n, BTStatic)) $ allVars decl
  in fromList pairs

-- create a proper division given the input and declaration
-- all variables are static except input variables with no given value
makeDiv :: Store -> VariableDecl -> EM Division
makeDiv store decl =
  do mapM_ onlyInput $ keys store
     let vars = allVars decl
         divlist = map (\n -> if isStatic n
                                then (n, BTStatic)
                                else (n, BTDynamic))
                        vars
     return $ fromList divlist
  where
    onlyInput n = if n `elem` input decl
                  then return ()
                  else Left $ "Variable \"" ++ n ++ "\" not in input"
    isStatic n = n `isIn` store || n `notElem` input decl

startingDiv :: (Ord a) => NormProgram a -> PWDivision a
                              -> Division
startingDiv (_, p) pwd =
  let n = nname $ getNEntryBlock p
  in fst $ get n pwd
