module GetEmBack where

import Control.Monad (Identity)
import MonadTransformers hiding (Identity)
import MaybeT
import EitherT
import ReaderT
import StateT

------------------------------------------------------------------
-- Using Identity can be great if we are working in something like
-- Scotty, where ReaderT is part of the environment:
--   + you can't easily retrieve the Reader
--   + you can't modify ReaderT without rewriting the library
--
-- Thus, using something like the following can be good to
-- maintain compatability with your library.
------------------------------------------------------------------

type Identity a = IdentityT Identity a
type Maybe    a = MaybeT    Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT r Identity a
type State  s a = StateT  s Identity a

