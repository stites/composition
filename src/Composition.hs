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

-- but notice that Compose already allows us to express arbitrarily nested types:
-- Compose

instance Applicative Identity where
  pure a = Identity a
  (Identity a) <*> (Identity b) = Identity ( a b )

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose ()



