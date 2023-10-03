module Values where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Control.Monad

type IntType = Word
type Name = String
type ErrMsg = String 
type EM = Either ErrMsg 

data Value = 
    Atom String
  | Num  IntType
  | Pair Value Value
  | Nil
  deriving (Eq, Show, Read, Ord) 

type Store = Map.Map Name Value

type Annotated l = (l, Maybe Store)

data Level = Static | Dynamic
  deriving (Eq, Show, Read)

getStore :: Annotated l -> Store
getStore = fromJust . snd

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
    Just v -> return v
    Nothing -> Left "Variable not found"

vars :: Store -> [Name]
vars = Map.keys

emptyStore :: Store
emptyStore = Map.empty

makeStore :: [(Name, Value)] -> Store
makeStore = Map.fromList

storeToList :: Store -> [(Name, Value)]
storeToList = Map.toAscList

update :: Name -> Value -> Store -> Store
update = Map.insert

updateWithStore :: Store -> Store -> Store
updateWithStore s1 s2 = Map.union s2 s1

without :: Store -> Name -> Store
without s n = Map.delete n s

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