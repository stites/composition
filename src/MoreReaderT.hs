module ReaderT.More where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ return . ((+) (-1))

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
                          putStrLn $ "Hi: " ++ show r
                          return   $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \r -> do
                           putStrLn $ "Hi: " ++ show r
                           return   $ (show r, r + 1)



