{-# LANGUAGE InstanceSigs #-}
module Composition where

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap.fmap) f fga

-- Generalize the above into different amounts of structure:
newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-- Two is already in Compose

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) =>
          Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap.fmap.fmap) f fgha

-- but notice that Compose already allows us to express
-- arbitrarily nested types:
-- Compose

instance Applicative Identity where
  pure a = Identity a
  Identity a <*> Identity b = Identity (a b)

instance Applicative f => Applicative (One f) where
  pure a = One $ pure a
  One f <*> One a = One $ f <*> a

instance (Applicative f, Applicative g) =>
          Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure    a =  Compose $ (pure.pure) a

  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  Compose f <*> Compose a = Compose $ (fmap (<*>) f) <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose a) = (foldMap.foldMap) f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse :: Applicative ap => (a -> ap b) -> Compose f g a -> ap (Compose f g b)
  traverse f (Compose a) = fmap Compose $ (traverse.traverse) f a
