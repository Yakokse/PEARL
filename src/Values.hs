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

type Store = Map.Map Name Value

trueV :: Value
trueV = Atom "true"

falseV :: Value
falseV = Nil

truthy :: Value -> Bool
truthy = (/= falseV)

boolify :: Bool -> Value
boolify b = if b then trueV else falseV

findErr ::  Store -> Name -> Value
findErr s n =
  case Map.lookup n s of
    Just v -> v
    _ -> error $ "static variable " ++ n ++ " not found during lookup"

onlyIn :: Store -> [Name] -> Store
onlyIn s ns = Map.filterWithKey (\k _ -> k `elem` ns) s

mapStore :: (Name -> Value -> Value) -> Store -> Store
mapStore = Map.mapWithKey

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
