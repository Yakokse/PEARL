module PE.Program2 where

import Utils.Error

import PE.AST2

getEntry' :: [Block' a] -> EM a
getEntry' p =
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right (name' b)
    _ -> Left "Multiple entry points found"
  where
    f b = case from' b of Entry' -> True; _ -> False

getEntryBlock' :: [Block' a] -> EM (Block' a)
getEntryBlock' p =
  case filter f p of
    [] -> Left "No entry point found"
    [b] -> Right b
    _ -> Left "Multiple entry points found"
  where
    f b = case from' b of Entry' -> True; _ -> False

getExitBlock' :: [Block' a] -> EM (Block' a)
getExitBlock' p =
  case filter isExit' p of
    [] -> Left "No exit point found"
    [b] -> Right b
    _ -> Left "Multiple exit points found"

getBlock' :: Eq a => [Block' a] -> a -> Maybe (Block' a)
getBlock' p l =
  case filter (\b -> name' b == l) p of
    [b] -> return b
    _   -> Nothing

getBlockUnsafe' :: Eq a => [Block' a] -> a -> Block' a
getBlockUnsafe' p l = head $ filter (\b -> name' b == l) p

getBlockErr' :: (Eq a, Show a) => [Block' a] -> a -> EM (Block' a)
getBlockErr' p l =
  case filter (\b -> name' b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ show l
    _   -> Left $ "Multiple blocks found named: " ++ show l
