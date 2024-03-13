module Utils.Maps where

import qualified Data.Map.Strict as Map

type Map = Map.Map

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

sets :: Ord n => [n] -> e -> Map n e -> Map n e
sets ns e m = foldl (\m' n -> set n e m') m ns

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

anyWhere :: (n -> e -> Bool) -> Map n e -> Bool
anyWhere f = not . null . allWhere f

mmap :: (n -> a -> b) -> Map n a -> Map n b
mmap = Map.mapWithKey
