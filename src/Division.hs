module Division where

import Values
import AST
import qualified Data.Map.Strict as Map
import Data.List (union)


type Division = Map.Map Name Level
type DivisionPW l = Map.Map l (Division, Division)

isType :: Name -> Level -> Division -> Bool
isType n t d =
  case d Map.! n of 
    r | t == r -> True
    _ -> False

getType :: Name -> Division -> Level
getType n d = d Map.! n

getDivs :: Ord a => a -> DivisionPW a -> (Division, Division)
getDivs a d = d Map.! a

setType :: Name -> Level -> Division -> Division
setType = Map.insert

boundedBy :: Name -> Level -> Division -> Division
boundedBy = Map.insertWith lub

setDiv :: Ord a => a -> (Division, Division) -> DivisionPW a -> DivisionPW a
setDiv = Map.insert 

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

listToPWDiv :: Ord l => [(l, (Division, Division))] -> DivisionPW l
listToPWDiv = Map.fromList

lubDiv :: [Division] -> Division
lubDiv = Map.unionsWith lub

makeStaticDiv :: VariableDecl -> Division
makeStaticDiv decl = 
  let ns = input decl `union` output decl `union` temp decl
      pairs = map (\n -> (n, BTStatic)) ns
  in listToDiv pairs

makeDiv :: Store -> VariableDecl -> EM Division
makeDiv store decl = 
  do mapM_ onlyInput names
     let allVars = input decl `union` output decl `union` temp decl
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
