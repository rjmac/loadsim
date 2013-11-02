module Client (Client(..), runClient) where

import Control.Monad.IO.Class
import Server
import Util

data Client m = Client {
  nextJobInµs :: m Int,
  pickServer :: [Server] -> m Server, 
  nextJob :: m Job }

runClient :: (MonadIO m) => Integer -> Client m -> [Server] -> m ()
runClient duration client servers = do
  start <- liftIO currentTime
  run' (start + duration) client servers

run' :: (MonadIO m) => Integer -> Client m -> [Server] -> m ()
run' deadline client servers = do
  nextJobIn <- nextJobInµs client
  now <- liftIO currentTime
  let nextJobAt = now + fromIntegral nextJobIn
  if nextJobAt > deadline
    then return ()
    else do
      liftIO $ sleepTo nextJobAt
      server <- (pickServer client) servers
      job <- nextJob client
      liftIO $ sendJob server job
      run' deadline client servers
