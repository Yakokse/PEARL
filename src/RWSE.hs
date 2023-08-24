module RWSE where

import Control.Monad

import Values

-- type ReadData a = Program' a
-- type WriteData a = Program a  -- must be an instance of Monoid
-- type StateData = Map.Map Name Value

-- a: Read, b: State, c: Write (Monoid), d : Value
newtype RWSE a b c d = RWSE {runRWSE :: a -> b -> 
  EM (d, c, b)}

instance (Monoid b, Monoid c) => Monad (RWSE a b c) where
  return = pure
  m >>= f = RWSE $ \rd sd -> do (v, w, s) <- runRWSE m rd sd
                                (v', w', s') <- runRWSE (f v) rd s
                                return (v', w `mappend` w', s')

instance (Monoid b, Monoid c) => Functor (RWSE a b c) where
  fmap = liftM
  
instance (Monoid b, Monoid c) => Applicative (RWSE a b c) where
  pure a = RWSE $ \_ sd -> Right (a, mempty, sd)
  (<*>) = ap

-- returns current read data
ask :: Monoid c => RWSE a b c a
ask = RWSE $ \r s -> Right (r, mempty, s) 

-- runs computation with new read data
with :: a -> RWSE a b c d -> RWSE a b c d
with r' m = RWSE $ \_ s -> runRWSE m r' s

-- write more data
tell :: c -> RWSE a b c ()
tell w = RWSE $ \_ sd -> Right ((), w, sd)

-- get state
get :: Monoid c => RWSE a b c b
get = RWSE $ \_ s -> Right (s, mempty, s)

-- replace state
put :: Monoid c => b -> RWSE a b c ()
put s' = RWSE $ \_ _ -> Right ((), mempty, s')

-- error
throw :: ErrMsg -> RWSE a b c d
throw e = RWSE $ \_ _ -> Left e

