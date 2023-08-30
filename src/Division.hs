module Division where

import Values
import qualified Data.Map.Strict as Map

data BTtype = Static | Dynamic 
  deriving (Eq, Show, Read) 

type Division = Map.Map Name BTtype

isDyn :: Name -> Division -> Bool
isDyn n d =
     case d Map.! n of 
      Dynamic -> True
      _ -> False

getType :: Name -> Division -> BTtype
getType n d = d Map.! n

setDyn :: Name -> Division -> Division
setDyn n = Map.insert n Dynamic

makeDyn :: [Name] -> Division -> Division
makeDyn ns d = foldl (flip setDyn) d ns

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