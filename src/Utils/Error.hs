module Utils.Error where

import Control.Monad

type ErrMsg = String
type EM = Either ErrMsg

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
