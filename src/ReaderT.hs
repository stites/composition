{-# LANGUAGE InstanceSigs #-}
module ReaderT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

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

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO



