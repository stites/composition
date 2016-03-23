{-# LANGUAGE InstanceSigs #-}
module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap.fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ (pure.pure) a
  (EitherT fnAToB) <*> (EitherT ma) = EitherT $ (fmap (<*>) fnAToB) <*> ma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Left  x -> return (Left x)
      Right y -> runEitherT (f y)



