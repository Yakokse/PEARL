module Values where

import qualified Data.Array as Array
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

type IntType = Word
type ArrayType = Array.Array IntType IntType
type StackType = [IntType]
type Name = String
type ErrMsg = String 
type EM = Either ErrMsg 

data Value = ScalarVal IntType | ArrVal ArrayType | StackVal StackType
    deriving (Eq, Show, Read) 
type Store = Map.Map Name Value

type Annotated l = (l, Maybe Store)

getStore :: Annotated l -> Store
getStore = fromJust . snd

truthy :: IntType -> Bool
truthy = (/= 0)

trueV :: IntType
trueV = 1

falseV :: IntType
falseV = 0

boolify :: Bool -> IntType
boolify b = if b then trueV else falseV

find ::  Name -> Store -> Maybe Value
find = Map.lookup

vars :: Store -> [Name]
vars = Map.keys

emptyStore :: Store
emptyStore = Map.empty

makeStore :: [(Name, Value)] -> Store
makeStore = Map.fromList

storeToList :: Store -> [(Name, Value)]
storeToList = Map.toAscList

valueToList :: Value -> [IntType]
valueToList (ScalarVal i) = [i]
valueToList (ArrVal a) = Array.elems a
valueToList (StackVal s) = s

update :: Name -> Value -> Store -> Store
update = Map.insert

without :: Store -> Name -> Store
without s n = Map.delete n s

remove :: [Name] -> Store -> Store
remove ns = Map.filterWithKey (\n _ -> n `notElem` ns) 

(!) :: ArrayType -> IntType -> IntType 
(!) = (Array.!)

listToArr :: [IntType] -> Value
listToArr l = ArrVal $ Array.array (0, toEnum $ length l - 1) pairs
  where pairs = zip [0..] l 

listToStack :: [IntType] -> Value
listToStack = StackVal

stackToList :: StackType -> [IntType]
stackToList = id

arrToList :: ArrayType -> [(IntType, IntType)]
arrToList = Array.assocs

updateIdx :: ArrayType -> IntType -> IntType -> ArrayType
updateIdx a idx val = a Array.// [(idx, val)]

