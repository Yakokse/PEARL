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
type Store = Map.Map Name Value

find ::  Name -> Store -> Maybe Value
find = Map.lookup

(!) :: ArrayType -> IntType -> IntType 
(!) = (Array.!)

update :: Name -> Value -> Store -> Store
update = Map.insert

without :: Store -> Name -> Store
without s n = Map.delete n s

updateIdx :: ArrayType -> IntType -> IntType -> ArrayType
updateIdx a idx val = a Array.// [(idx, val)]