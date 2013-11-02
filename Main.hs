{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent.STM
import Data.Foldable
import Data.Aeson
import System.Random

import Server
import Util
import DataLog
import Client

-- Number of server processes.  Each server can contain an unbounded
-- number of workers, which are created on demand
serverCount :: Int
serverCount = 5

-- Baseline load for each server
baselineLoad :: Int
baselineLoad = 0

-- How often each server updates its publically-visible load value
loadUpdateµs :: Integer
loadUpdateµs = 1000000

-- Total time to run the simulation
runtimeµs :: Integer
runtimeµs = 5000000

leastLoad :: [Server] -> STM Server
leastLoad = minByM serverLoad

uniformRand :: Int -> IO Int
uniformRand i = getStdRandom (randomR (0, i - 1))

stdJob :: IO Job
stdJob =
  Job <$> pure 1 <*> uniformRand 10000

randomClient :: Client IO
randomClient = Client {
  nextJobInµs = uniformRand 1000,
  pickServer = \servers -> (servers !!) <$> (uniformRand (length servers)),
  nextJob = stdJob
  }

lower :: Server -> Server -> IO Server
lower a b = do
  (loadOfA, loadOfB) <- atomically ((,) <$> serverLoad a <*> serverLoad b)
  if loadOfA < loadOfB
    then return a
    else if loadOfB < loadOfA
         then return b
         else do
           coin <- getStdRandom (randomR (False, True))
           if coin then return a else return b

randomLowerOfTwo :: Client IO
randomLowerOfTwo = Client {
  nextJobInµs = uniformRand 1000,
  pickServer = (\servers -> do
    let n = length servers                   
    a <- (servers!!) <$> (uniformRand n)
    b <- (servers!!) <$> (uniformRand n)
    lower a b),
  nextJob = stdJob
  }

overallLeast :: Client IO
overallLeast = Client {
  nextJobInµs = uniformRand 1000,
  pickServer = atomically . leastLoad,
  nextJob = stdJob
  }

main :: IO ()
main = withDataLog "datalog.json" process
  where
    process dataLog = do
      servers <- mapM (newServer loadUpdateµs baselineLoad (onLoadChange dataLog)) [1..serverCount]
      runClient runtimeµs randomLowerOfTwo servers
      traverse_ stopServer servers
      traverse_ waitForServer servers
    onLoadChange dataLog sid old new =
      writeToLog dataLog $ object [ "sid" .= sid, "old" .= old, "new" .= new ]
