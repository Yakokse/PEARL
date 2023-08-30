module Values where

import qualified Data.Array as Array
import qualified Data.Map.Strict as Map

type IntType = Word
type ArrayType = Array.Array IntType IntType
type StackType = [IntType]
type Name = String
type ErrMsg = String 
type EM = Either ErrMsg 

data Value = ScalarVal IntType | ArrVal ArrayType | StackVal StackType
    deriving (Eq, Show, Read) 
type Store = Map.Map Name Value

truthy :: IntType -> Bool
truthy = (/= 0)

trueV :: IntType
trueV = 1

falseV :: IntType
falseV = 0

find ::  Name -> Store -> Maybe Value
find = Map.lookup

(!) :: ArrayType -> IntType -> IntType 
(!) = (Array.!)

emptyStore :: Store
emptyStore = Map.empty

update :: Name -> Value -> Store -> Store
update = Map.insert

without :: Store -> Name -> Store
without s n = Map.delete n s

updateIdx :: ArrayType -> IntType -> IntType -> ArrayType
updateIdx a idx val = a Array.// [(idx, val)]

