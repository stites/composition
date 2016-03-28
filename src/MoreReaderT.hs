module ReaderT.More where

import Control.Monad.Trans.Reader
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ return . ((+) (-1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show



