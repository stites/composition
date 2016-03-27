module OuterInner where

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1 -- we can consider this return as a type-composition

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- now if we want to evaluate the code, we can feed in a unit:
-- readerUnwrap ()

-- alternatively, iw you consider the return composed of reader, either and maybe:
{-
instance Monad ((->) r) where
  return = const
instance Monad (Either e) where
  return = Right
instance Monad (Maybe) where
  return = Just
-}
-- then we can get the same result with:
-- (const . Right . Just $ 1) ()

-- "Base Monad" is usually in reference to the outermost monad.
-- type MyType a = IO [Maybe a]  has a base monad of IO.

given :: b -> Either String (Maybe Integer)
given = const (Right (Just 1))

returnIO :: a -> IO a
returnIO = return

firstLayer :: ReaderT () IO (Either String (Maybe Integer))
firstLayer = ReaderT $ fn $ given
    where fn :: (() -> Either String (Maybe Integer)) -> () -> IO (Either String (Maybe Integer))
          fn f _ = returnIO $ f ()

secondLayer :: MaybeT (ReaderT () IO) Integer
secondLayer = MaybeT $ fn given
    where fn :: (() -> Either String (Maybe Integer)) -> ReaderT () IO (Maybe Integer)
          fn f = case f () of
                   Right x -> ReaderT $ (const . returnIO) x
                   Left _  -> ReaderT $ (const . returnIO) Nothing

thirdLayer :: MaybeT (ExceptT String (ReaderT () IO)) Integer
thirdLayer = MaybeT $ fn $ given
    where fn :: (() -> Either String (Maybe Integer)) -> ExceptT String (ReaderT () IO) (Maybe Integer)
          fn = (ExceptT . ReaderT . const . returnIO . (\f -> f ()))

reembedded :: MaybeT (ExceptT String (ReaderT () IO)) Integer
reembedded = MaybeT . ExceptT . ReaderT . const . returnIO . (\f -> f ()) $ given



