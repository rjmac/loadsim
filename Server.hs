module Server (
  Job(..),
  Server,
  Advertisement(..),
  newServer,
  sendJob,
  stopServer,
  waitForServer,
  serverLoad
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Reader
import Data.Set as S
import Control.Exception

import Util

data Job = Job { heaviness :: Int
               , durationµs :: Int }
         deriving (Show, Read, Eq, Ord)

data JobQueueEntry = JobEntry Job
                   | Quit
                   deriving (Show, Read, Eq)

data Advertisement = Advertisement { advertisedLoad :: Int
                                   , advertisedTimeµs :: Integer
                                   }
                   deriving (Show, Read, Eq)

fetchJob :: TChan JobQueueEntry -> STM JobQueueEntry
fetchJob jobQueue = do
  jobEntry <- readTChan jobQueue
  when (jobEntry == Quit)
    (writeTChan jobQueue Quit)
  return jobEntry

waitForEmpty :: TVar (Set a) -> STM ()
waitForEmpty set = do
  s <- readTVar set
  if S.null s
    then return ()
    else retry

waitForItem :: Ord a => a -> TVar (Set a) -> STM ()
waitForItem item set = do
  s <- readTVar set
  if member item s
    then return ()
    else retry

doJob :: Job -> TVar Int -> (Int -> Int -> IO ()) -> IO ()
doJob job loadVar onLoadChange = do
  updateLoad (heaviness job)
  threadDelay $ durationµs job
  updateLoad (- heaviness job)
  where
    updateLoad delta = do
      (oldLoad, newLoad) <- atomically $ do
        oldLoad <- readTVar loadVar
        let newLoad = oldLoad + delta
        writeTVar loadVar newLoad
        return (oldLoad, newLoad)
      onLoadChange oldLoad newLoad

data ServerState = ServerState {
    myId :: Int
  , myQueue :: TChan JobQueueEntry
  , myWorkers :: TVar (Set ThreadId)
  , myLoad :: TVar Int
  , myLoadMonitor :: ThreadId
  , myOnLoadChange :: Int -> Int -> IO ()
  }

type ServerAction a = ReaderT ServerState IO a

loadMonitorProcess :: Int -> Integer -> TVar Int -> TVar Advertisement -> IO ()
loadMonitorProcess sId µs trueLoad visibleLoad = forever $ do
  logit sId "Updating visible state"
  now <- currentTime
  atomically $ readTVar trueLoad >>= (\l -> writeTVar visibleLoad $ Advertisement { advertisedLoad = l, advertisedTimeµs = now })
  sleep µs

newServerState :: Int -> TChan JobQueueEntry -> Integer -> TVar Advertisement -> (Int -> Int -> Int -> IO ()) -> IO ServerState
newServerState sId jobQueue loadUpdateµs visibleLoad onLoadChange = do
  sp <- newTVarIO S.empty
  l <- atomically $ readTVar visibleLoad >>= newTVar . advertisedLoad
  lm <- forkIO $ loadMonitorProcess sId loadUpdateµs l visibleLoad
  return $ ServerState { myId = sId
                       , myQueue = jobQueue
                       , myWorkers = sp
                       , myLoad = l
                       , myLoadMonitor = lm
                       , myOnLoadChange = onLoadChange sId
                       }

startJob :: Job -> ServerAction ()
startJob job = do
  loadVar <- myLoad <$> ask
  subProcs <- myWorkers <$> ask
  onLoadChange <- myOnLoadChange <$> ask
  liftIO $ do
    child <- forkIO $ do
      me <- myThreadId
      atomically $ waitForItem me subProcs
      doJob job loadVar onLoadChange
      atomically $ modifyTVar subProcs (delete me)
    atomically $ modifyTVar subProcs (insert child)

process :: ServerAction ()
process = do
  jobQueue <- myQueue <$> ask
  job <- liftIO $ atomically $ fetchJob jobQueue
  handleQueueEntry job

serverLog :: String -> ServerAction ()
serverLog msg = do
  sid <- myId <$> ask
  logit sid msg

handleQueueEntry :: JobQueueEntry -> ServerAction ()
handleQueueEntry (JobEntry j) = do
  serverLog "got job"
  startJob j
  process
handleQueueEntry Quit = do
  serverLog "shutting down"
  subProcs <- myWorkers <$> ask
  loadMonitor <- myLoadMonitor <$> ask
  liftIO $ do
    atomically $ waitForEmpty subProcs
    killThread $ loadMonitor

serverMain :: ServerAction ()
serverMain = do
  serverLog "starting"
  process
  serverLog "exiting"

serverProcess :: Int -> Integer -> TChan JobQueueEntry -> TVar Advertisement -> (Int -> Int -> Int -> IO ()) -> IO ()
serverProcess sId loadUpdateµs jobQueue visibleLoad onLoadChange =
  newServerState sId jobQueue loadUpdateµs visibleLoad onLoadChange >>= runReaderT serverMain

data Server = Server {
    serverId :: Int
  , serverQueue :: TChan JobQueueEntry
  , serverVisibleLoad :: TVar Advertisement
  , serverThread :: ThreadId
  , serverTermination :: TMVar ()
  }

stopServer :: Server -> IO ()
stopServer s = atomically $ writeTChan (serverQueue s) Quit

sendJob :: Server -> Job -> IO ()
sendJob s j = atomically $ writeTChan (serverQueue s) (JobEntry j)

waitForServer :: Server -> IO ()
waitForServer s = atomically $ takeTMVar (serverTermination s)

newServer :: Integer -> Int -> (Int -> Int -> Int -> IO ()) -> Int -> IO Server
newServer loadUpdateµs baselineLoad onLoadChange sId = do
  queue <- newTChanIO
  now <- currentTime
  visibleLoad <- newTVarIO $ Advertisement { advertisedLoad = baselineLoad, advertisedTimeµs = now }
  termination <- newEmptyTMVarIO
  thread <- forkIO $ finally (serverProcess sId loadUpdateµs queue visibleLoad onLoadChange) (atomically $ putTMVar termination ())
  return $ Server { serverId = sId
                  , serverQueue = queue
                  , serverVisibleLoad = visibleLoad
                  , serverThread = thread
                  , serverTermination = termination 
                  }
serverLoad :: Server -> STM Advertisement
serverLoad = readTVar . serverVisibleLoad

