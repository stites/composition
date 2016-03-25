{-# LANGUAGE InstanceSigs #-}
module ReaderT where

-- this is just like Reader, but we get additional
-- structure for doing stuff!

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap.fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ (pure.pure) a
  (ReaderT fab) <*> (ReaderT ma) = ReaderT $ ((<*>) <$> fab) <*> ma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a {-::a-} <- rma r {-::m a-}
    (runReaderT $ f a {-::ReaderT r m b-}) {-::r -> m b-} r






