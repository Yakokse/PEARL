module Values where

import qualified Data.Map.Strict as Map
import Control.Monad

type IntType = Word
type Name = String
type ErrMsg = String 
type EM = Either ErrMsg 
type Label = String


data Value = 
    Atom String
  | Num  IntType
  | Pair Value Value
  | Nil
  deriving (Eq, Show, Read, Ord) 

data SpecValue = Dynamic | Static Value
  deriving (Eq, Show, Read, Ord)

type Store = Map.Map Name SpecValue

data Level = BTStatic | BTDynamic
  deriving (Eq, Show, Read)

lub :: Level -> Level -> Level
lub BTStatic BTStatic = BTStatic
lub _ _ = BTDynamic

trueV :: Value
trueV = Atom "true"

falseV :: Value
falseV = Nil

truthy :: Value -> Bool
truthy = (/= falseV)

boolify :: Bool -> Value
boolify b = if b then trueV else falseV

find ::  Name -> Store -> EM Value
find n s = 
  case Map.lookup n s of 
    Just (Static v) -> return v
    Just _ -> Left $ "Variable \"" ++ n ++ "\" dynamic during lookup"
    _ -> Left $ "Variable \"" ++ n ++ "\" not found during lookup"

findDyn ::  Name -> Store -> EM Value
findDyn n s = 
  case Map.lookup n s of 
    Just (Static v) -> return v
    Just _ -> return Nil
    _ -> Left $ "Variable \"" ++ n ++ "\" not found during lookup"

find' :: Name -> Store -> SpecValue
find' = Map.findWithDefault Dynamic

vars :: Store -> [Name]
vars = Map.keys

emptyStore :: Store
emptyStore = Map.empty

makeStore :: [(Name, SpecValue)] -> Store
makeStore = Map.fromList

storeToList :: Store -> [(Name, SpecValue)]
storeToList = Map.toAscList

update :: Name -> SpecValue -> Store -> Store
update = Map.insert

updateWithStore :: Store -> Store -> Store
updateWithStore s1 s2 = Map.union s2 s1

without :: Store -> Name -> Store
without s n = Map.delete n s

mapStore :: (Name -> SpecValue -> SpecValue) -> Store -> Store
mapStore = Map.mapWithKey

isIn :: Name -> Store -> Bool
isIn = Map.member

remove :: [Name] -> Store -> Store
remove ns = Map.filterWithKey (\n _ -> n `notElem` ns) 

newtype LEM a = LEM {runLEM :: (EM a, [String])}

instance Monad LEM where
  return = pure 
  LEM (v, l) >>= f = 
    LEM $ case v of
      Left e -> (Left e, l)
      Right res -> let LEM (v', l') = f res in (v', l ++ l')
instance Functor LEM where
  fmap = liftM
instance Applicative LEM where
  pure a = LEM (Right a, []); (<*>) = ap

raise :: EM a -> LEM a 
raise em = LEM (em, [])

logM :: String -> LEM ()
logM s = LEM (Right (), [s])

logManyM :: [String] -> LEM ()
logManyM = mapM_ logM

emToLEM :: EM a -> LEM a
emToLEM m = LEM $ case m of
  Left e -> (Left e, [e])
  Right s -> (Right s, [])
  