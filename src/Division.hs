module Division where

import Values
import AST
import Utils
import qualified Data.Map.Strict as Map
 

type Division = Map.Map Name Level
type DivisionPW l = Map.Map l Division

isType :: Name -> Level -> Division -> Bool
isType n t d =
  case d Map.! n of 
    r | t == r -> True
    _ -> False

getType :: Name -> Division -> Level
getType n d = d Map.! n

getDiv :: Ord a => a -> DivisionPW a -> Division
getDiv a d = d Map.! a

setType :: Name -> Level -> Division -> Division
setType = Map.insert

boundBy :: Name -> Level -> Division -> Division
boundBy = Map.insertWith lub

setDiv :: Ord a => a -> Division -> DivisionPW a -> DivisionPW a
setDiv =  Map.insert

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

listToDiv :: [(Name, Level)] -> Division
listToDiv = Map.fromList

listToPWDiv :: Ord l => [(l, Division)] -> DivisionPW l
listToPWDiv = Map.fromList

lubDiv :: [Division] -> Division
lubDiv = Map.unionsWith lub

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