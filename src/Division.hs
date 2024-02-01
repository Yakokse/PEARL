module Division where

import Values
import AST
import Utils
import qualified Data.Map.Strict as Map
 

type Division = Map.Map Name Level
type DivisionPW = Map.Map Label Division

isType :: Name -> Level-> Division -> Bool
isType n t d =
  case d Map.! n of 
    r | t == r -> True
    _ -> False

getType :: Name -> Division -> Level
getType n d = d Map.! n

setType :: Name -> Level -> Division -> Division
setType = Map.insert

setTypes :: [Name] -> [Level] -> Division -> Division
setTypes ns ts bDiv = foldl (\d (n, t) -> setType n t d) bDiv pairs
  where pairs = zip ns ts

allDyn :: Division -> [Name]
allDyn = map fst . filter ((== BTDynamic) . snd) . Map.toList

allStatic :: Division -> [Name]
allStatic = map fst . filter ((== BTStatic) . snd) . Map.toList

defaultDivision :: Division
defaultDivision = Map.empty

divisionToList :: Division -> [(Name, Level)]
divisionToList = Map.toAscList

makeDiv :: Store -> VariableDecl -> EM Division
makeDiv store decl = 
  do mapM_ onlyInput names
     let allVars = getVarsDecl decl
         allTypes = map (\n -> if isStatic n 
                              then BTStatic 
                              else BTDynamic) 
                        allVars
     return $ setTypes allVars allTypes defaultDivision
  where 
    onlyInput n = if n `elem` input decl 
                  then return () 
                  else Left $ "Variable \"" ++ n ++ "\" not in input"
    storelist = storeToList store
    names = map fst storelist
    isStatic n = n `isIn` store || n `notElem` input decl