module Division where

import Values
import AST
import qualified Data.Map.Strict as Map
import Data.List (union)


type Division = Map.Map Name Level
type DivisionPW l = Map.Map l (Division, Division)

-- check if variable is given type in division
isType :: Name -> Level -> Division -> Bool
isType n t d = getType n d == t

-- get type of variable in division
getType :: Name -> Division -> Level
getType n d = d Map.! n

-- get initial and final division of a label in the PW division
getDivs :: Ord a => a -> DivisionPW a -> (Division, Division)
getDivs a d = d Map.! a

-- set the type of a variable in a division
setType :: Name -> Level -> Division -> Division
setType = Map.insert

-- bound the variable in a division 
boundedBy :: Name -> Level -> Division -> Division
boundedBy = Map.insertWith lub

-- set the divisions for a given label in a PW division
setDiv :: Ord a => a -> (Division, Division) -> DivisionPW a -> DivisionPW a
setDiv = Map.insert 

-- set the types of multiple variables in a division
setTypes :: [Name] -> [Level] -> Division -> Division
setTypes ns ts bDiv = foldl (\d (n, t) -> setType n t d) bDiv pairs
  where pairs = zip ns ts

-- get all variables of a specific type in a division
allOfType :: Level -> Division -> [Name]
allOfType l = map fst . filter ((== l) . snd) . Map.toList

-- the empty division
defaultDivision :: Division
defaultDivision = Map.empty

-- turn a division into an associative list
divisionToList :: Division -> [(Name, Level)]
divisionToList = Map.toAscList

-- turn an associative list into a division
listToDiv :: [(Name, Level)] -> Division
listToDiv = Map.fromList

-- turn an associative list into a PW division
listToPWDiv :: Ord l => [(l, (Division, Division))] -> DivisionPW l
listToPWDiv = Map.fromList

-- the least-upper-bound of a list of divisions
lubDiv :: [Division] -> Division
lubDiv = Map.unionsWith lub

-- make a division for all variables in a declaration
-- sets all variables to static
makeStaticDiv :: VariableDecl -> Division
makeStaticDiv decl = 
  let ns = input decl `union` output decl `union` temp decl
      pairs = map (\n -> (n, BTStatic)) ns
  in listToDiv pairs

-- create a proper division given the input and declaration
-- all variables are static except input variables with no given value
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
