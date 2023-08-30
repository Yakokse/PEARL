module Division where

import Values
import qualified Data.Map.Strict as Map

data BTtype = Static | Dynamic 
  deriving (Eq, Show, Read) 

type Division = Map.Map Name BTtype

isType :: Name -> BTtype -> Division -> Bool
isType n t d =
     case d Map.! n of 
      r | t == r -> True
      _ -> False

getType :: Name -> Division -> BTtype
getType n d = d Map.! n

setType :: Name -> BTtype -> Division -> Division
setType = Map.insert

setTypes :: [Name] -> [BTtype] -> Division -> Division
setTypes ns ts bDiv = foldl (\d (n, t) -> setType n t d) bDiv pairs
  where pairs = zip ns ts

allDyn :: Division -> [Name]
allDyn = map fst . filter ((== Dynamic) . snd) . Map.toList

allStatic :: Division -> [Name]
allStatic = map fst . filter ((== Static) . snd) . Map.toList

defaultDivision :: Division
defaultDivision = Map.empty

makeDiv :: [Name] -> [Name] -> Division
makeDiv spec total = Map.fromList pairs
    where 
        pairs = zip total bttype
        bttype = map (\n -> if n `elem` spec then Static else Dynamic) total