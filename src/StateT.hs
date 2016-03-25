{-# LANGUAGE InstanceSigs #-}
module StateT where

import Data.Tuple (swap)
-- the strict StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ \s -> case sma s of
    ma -> fmap swap $ (fmap.fmap) f (fmap swap ma)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (  a, _) <- sma s
    (fab, _) <- smab s
    return (fab a, s)

