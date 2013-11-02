module Util(
  sleepTo,
  sleep,
  logit,
  minByM,
  currentTime
) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import System.Clock
import Data.Function
import Text.Printf
import Data.List

logit :: (MonadIO m) => Int -> String -> m ()
logit identifier msg = liftIO $ printf "Server %d: %s\n" identifier msg

currentTime :: IO Integer
currentTime = asµs <$> getTime Monotonic
  where 
    asµs :: TimeSpec -> Integer
    asµs (TimeSpec s ns) =
      (fromIntegral s) * 1000000 + (fromIntegral ns) `div` 1000

sleepTo :: Integer -> IO ()
sleepTo time = do
  start <- currentTime
  sleepTo' time start
  
sleep :: Integer -> IO ()
sleep µs = do
  start <- currentTime
  sleepTo' (start + µs) start

sleepTo' :: Integer -> Integer -> IO ()
sleepTo' deadline now =
  if now < deadline
   then do
     pause (deadline - now)
     currentTime >>= sleepTo' deadline
   else return ()
  where
    limit :: Int
    limit = maxBound `div` 4 -- divide in an attempt to avoid overflows
    pause :: Integer -> IO ()
    pause duration = do
      if duration > fromIntegral limit
        then threadDelay limit
        else threadDelay $ fromIntegral duration

decorateM :: (Monad m) => (a -> m b) -> [a] -> m [(a, b)]
decorateM f xs =
  mapM (\x -> f x >>= \y -> return (x, y)) xs

minByM :: (Monad m, Ord b) => (a -> m b) -> [a] -> m a
minByM f xs = do
  decorated <- decorateM f xs
  return $ fst $ minimumBy (compare `on` snd) $ decorated

