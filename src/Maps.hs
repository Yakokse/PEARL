module Maps where

import qualified Data.Map.Strict as Map

type Map = Map.Map
-- type VarMap e = Map Name e
-- type PWMap l e = Map l (VarMap e, VarMap e)
-- type Store = VarMap Value
-- type SpecStore = VarMap SpecValue

get :: Ord n => n -> Map n e -> e
get n m = m Map.! n

set :: Ord n => n -> e -> Map n e -> Map n e
set = Map.insert

update :: Ord n => (e -> e -> e) -> n -> e -> Map n e -> Map n e
update = Map.insertWith

combine :: Ord n => Map n e -> Map n e -> Map n e
combine = Map.union

combineWith :: Ord n => (e -> e -> e) -> [Map n e] -> Map n e
combineWith = Map.unionsWith

sets :: Ord n => [n] -> [e] -> Map n e -> Map n e
sets ns es m = foldl (\m' (n, t) -> set n t m') m pairs
  where pairs = zip ns es

emptyMap :: Map n e
emptyMap = Map.empty

isIn :: Ord n => n -> Map n e -> Bool
isIn = Map.member

toList :: Map n e -> [(n,e)]
toList = Map.toAscList

fromList :: Ord n => [(n,e)] -> Map n e
fromList = Map.fromList

disjoint :: Ord n =>  Map n e -> Map n e -> Bool
disjoint = Map.disjoint

without :: Ord n => Map n e -> n -> Map n e
without = flip Map.delete

withouts :: Ord n => Map n e -> [n] -> Map n e
withouts = foldl without

keys :: Map n e -> [n]
keys = Map.keys

lookupM :: Ord n => n -> Map n e -> Maybe e
lookupM = Map.lookup

lookup' :: Ord n => e -> n -> Map n e -> e
lookup' = Map.findWithDefault

mfilter :: (e -> Bool) -> Map n e ->  Map n e
mfilter = Map.filter

allWhere :: (n -> e -> Bool) -> Map n e -> [n]
allWhere f m = keys $ Map.filterWithKey f m

mmap :: (n -> a -> b) -> Map n a -> Map n b
mmap = Map.mapWithKey
-- make n division for all variables in n declaration
-- sets all variables to static
-- makeStaticDiv :: VariableDecl -> Division
-- makeStaticDiv decl =
--   let ns = input decl `union` output decl `union` temp decl
--       pairs = map (\n -> (n, BTStatic)) ns
--   in listToDiv pairs

-- create n proper division given the input and declaration
-- all variables are static except input variables with no given value
-- makeDiv :: Store -> VariableDecl -> EM Division
-- makeDiv store decl =
--   do mapM_ onlyInput names
--      let allVars = input decl `union` output decl `union` temp decl
--          allTypes = map (\n -> if isStatic n
--                               then BTStatic
--                               else BTDynamic)
--                         allVars
--      return $ setTypes allVars allTypes defaultDivision
--   where
--     onlyInput n = if n `elem` input decl
--                   then return ()
--                   else Left $ "Variable \"" ++ n ++ "\" not in input"
--     storelist = storeToList store
--     names = map fst storelist
--     isStatic n = n `isIn` store || n `notElem` input decl
