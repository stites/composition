module More.MonadTs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad

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

isValid :: String -> Bool
isValid v = '!' `elem` v

hungryText :: MaybeT IO String
hungryText = do
  v <- lift getLine
  guard (isValid v)
  return v

main :: IO ()
main = do
  putStrLn "say something."
  excite <- runMaybeT hungryText
  case excite of
    Nothing -> putStrLn "Nothing! Nothing was said!"
    Just e  -> putStrLn ("Boom! what was said: " ++ e)

