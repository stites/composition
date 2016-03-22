{-# LANGUAGE InstanceSigs #-}

module MonadTransformers where
{-
Monads are a problem! you can't compose them like you can with applicatives and functors! Otherwise you get stuff that looks like:

instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure
  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  (>>=) = ???

but because f and g are both monads, they come with baggage:

Monad f => f a -> (f -> f b) -> f b
Monad g => g a -> (g -> g b) -> g b

so we'd get something like:

(Monad f, Monad g) => f (g a) -> (a -> f (g b)) -> f (g b)

or, collapsing this down: f (g (f (g a))) -> f (g a)

and there is no way to garuntee that we can `join` the final f and g. Thus, we must move to Monad Transformers.

A monad transformer is a type constructor that takes a monad as an argument and returns a monad as a result. Since the problem with composing two monads lies in the fact that we can't `join` two unknown monads, we have to scope down our polymorphism to get concrete information about one of the moands that we're working with.

We only need one monad to be concrete in order to perform the join and we can keep
the second one as a polymorphic type argument. With this kind of strategy we can use
monad transformers to account for two or more types that all have monad instances.
 -}

{- applicative lets us apply functions of more than one argummment when we have structure so that we get things like this:

fmap (+1) (Just 1)

and turn them into
(,,) <$> Just (1::Double) <*> Just "lol" <*> Just [1,2::Integer]

likewise, we want to be able to handle multiple monads at a time, an example of this is a Reader and IO monad - which is common in web applications: IO, perhaps, for talking to a database, and Reader for the database connection(s) and/or HTTP request context. We want something like a "big bind"

IO (Reader String [a])
-}

-- take 1:
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a = MaybeList { runMaybeList :: [Maybe a] }

-- monad transformers allow us not to do this one-off monad for every combination of types.

-- starting simple:
newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor f => Functor (IdentityT f) where
  fmap g (IdentityT fa) = IdentityT (fmap g fa)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
  pure a = IdentityT $ pure a
  IdentityT fab <*> IdentityT fa = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT  m b
  -- (>>=) (IdentityT ma)   (f                 ) = IdentityT $ ma >>= (runIdentityT . f)
  -- ^ notice that the second bind is from m to get ma to unwrap itself!
  -- ^ we have to run the identity because we f returns an IdentityT
  IdentityT ma >>= f = IdentityT $ ma >>= (runIdentityT . f)
--IdentityT ma >>= f = let aInMonadB = fmap f ma :: m (IdentityT m b)
--                     in ...doesn't work! we can't join the above!
-- | we do know, however, that IdentityT is a monad, so we can runIdentityT for:
--
--IdentityT ma >>= f = let aInMonadB = fmap runIdentityT (fmap f ma) :: m (m b)
--                     in ...closer!
--
--IdentityT ma >>= f = let aInMonadB :: m b
--                         aInMonadB = join $ fmap runIdentityT (fmap f ma)
--                     -- ...there, but we _actually_ want an `IdentityT m b`. Thus:
--                     in IdentityT aInMonadB
-- tada!

-- let's refactor:
--   IdentityT ma >>= f = IdentityT $ join $ fmap runIdentityT (fmap f ma)
--
-- functor law: fmap (f . g) == fmap f . fmap g
--   IdentityT ma >>= f = IdentityT $ join $ fmap (runIdentityT . f) ma
--
-- join composed with fmap is >>=: x >>= f = join $ fmap f x
--   IdentityT ma >>= f = IdentityT $ ma >>= runIdentityT . f
--
-- in the transformers library, this is:
--   m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m

-- Notice that the special sauce was in the fact that we required `runIdentityT`,
-- which is, essentially, fmapping a fold of the ItentityT structure, and then
-- we repacked the value into an IdentityT. This was to avoid the nested IdentityT!


-- now we can do things like combine IdentityT with List!
itWorks :: IdentityT [] Int
itWorks = IdentityT [1,2,3::Int] >>= (return . (+1))



