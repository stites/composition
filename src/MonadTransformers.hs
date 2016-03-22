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
 -}


