{-# LANGUAGE InstanceSigs #-}
module Composition where

import Prelude hiding (Either(..))

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

-- has nothing to do with the above:

class Bifunctor p where
  {-# MINIMAL bimap | first , second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b
data Const a b = Const a
data Drei a b c = Drei a b c
data SuperDrei a b c = SuperDrei a b
data SemiDrei a b c = SemiDrei a
data Quadriceps a b c d = Quadriceps a b c d
data Either a b = Left a | Right b

instance Bifunctor Deux where
  first  f (Deux a b) = Deux (f a)   b
  second f (Deux a b) = Deux    a (f b)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (g d)

instance Bifunctor Either where
  bimap f _ (Left a)  = Left  (f a)
  bimap _ g (Right b) = Right (g b)

