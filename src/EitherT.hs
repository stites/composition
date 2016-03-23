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

swapEither :: Either e a -> Either a e
swapEither (Left  x) = Right x
swapEither (Right x) = Left  x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT ema = EitherT $ fmap swapEither (runEitherT ema)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT aToMC bToMC eamb = runEitherT eamb >>= either aToMC bToMC



