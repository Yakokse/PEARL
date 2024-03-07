module RL.Program where

import RL.AST

import Utils.Error

exitCount :: [Block a b] -> Int
exitCount = length . filter isExit

nameIn :: (Eq a, Eq b) => (a,b) -> [Block a b] -> Bool
nameIn l = any (\b -> name b == l)

getBlock :: (Eq a, Eq b) => [Block a b] -> (a, b) -> Maybe (Block a b)
getBlock p l =
  case filter (\b -> name b == l) p of
    [b] -> return b
    _   -> Nothing

getNBlock :: Eq a => [NormBlock a] -> a -> NormBlock a
getNBlock p l = head $ filter (\b -> nname b == l) p

getBlockUnsafe :: (Eq a, Eq b) => [Block a b] -> (a, b) -> Block a b
getBlockUnsafe p l = head $ filter (\b -> name b == l) p

getBlockErr :: (Eq a, Eq b, Show a, Show b) =>
               [Block a b] -> (a, b) -> EM (Block a b)
getBlockErr p l =
  case filter (\b -> name b == l) p of
    [b] -> return b
    []  -> Left $ "Block not found: " ++ show l
    _   -> Left $ "Multiple blocks found named: " ++ show l

getEntryBlock :: [Block a b] -> EM (Block a b)
getEntryBlock p =
  case filter isEntry p of
    [] -> Left "No entry point found"
    [b] -> Right b
    _ -> Left "Multiple entry points found"

getNEntryBlock :: [NormBlock a] -> NormBlock a
getNEntryBlock p = head $ filter isNEntry p

getEntryLabel :: [Block a b] -> a
getEntryLabel = label . head . filter isEntry

getEntryName :: [Block a b] -> (a, b)
getEntryName = name . head . filter isEntry

getEntry :: [Block a b] -> EM (a,b)
getEntry p = name <$> getEntryBlock p

getExitBlock :: [Block a b] -> EM (Block a b)
getExitBlock p =
  case filter isExit p of
    [] -> Left "No exit point found"
    [b] -> Right b
    _ -> Left "Multiple exit points found"

getNExitBlock :: [NormBlock a] -> NormBlock a
getNExitBlock p = head $ filter isNExit p

getExitLabel :: [Block a b] -> a
getExitLabel = label . head . filter isExit

isNEntry :: NormBlock a -> Bool
isNEntry = isFromEntry . nfrom

isNExit :: NormBlock a -> Bool
isNExit = isJumpExit . njump
