{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Cook.Ci.Dequeue where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Cook.Ci.Types
import Data.Aeson
import Data.List
import Data.Ord
import Network.Wreq hiding (delete)
import Test.Framework
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

downloadBinary :: DownloadUrl -> IO BSL.ByteString
downloadBinary (DownloadUrl url) =
    do r <- get (T.unpack url)
       return (r ^. responseBody)

data DequeueIf
   = DequeueIf
   { di_pollJobs :: IO [Job]
   , di_callbackHook :: JobState -> IO ()
   }

webDequeueIf :: Config -> DequeueIf
webDequeueIf cfg =
    DequeueIf
    { di_pollJobs =
          do r <- asJSON =<< get (T.unpack $ unPollUrl $ c_pollUrl cfg)
             return (r ^. responseBody)
    , di_callbackHook =
        \jobSt ->
            do r <- post (T.unpack $ unCallbackUrl $ c_callbackUrl cfg) (toJSON jobSt)
               let respCode = r ^. responseStatus . statusCode
               when (respCode /= 200) $
                    putStrLn "Callback-URL didn't respond with status 200!"
    }

launchDequeue :: Config -> (Job -> IO (Bool, T.Text)) -> IO ()
launchDequeue cfg jobHandler =
    do st <- CiState <$> newTVarIO []
       launchDequeue' (webDequeueIf cfg) st cfg jobHandler

launchDequeue' :: DequeueIf -> CiState -> Config -> (Job -> IO (Bool, T.Text)) -> IO ()
launchDequeue' dequeueIf ciState cfg jobHandler =
    do jobListOrig <- di_pollJobs dequeueIf
       currentJobs <- atomically $ readTVar (cs_processing ciState)
       let jobList = filter (\j -> not $ j_id j `elem` currentJobs) jobListOrig
           freeSlots = (c_workers cfg) - (length currentJobs)
           enqueueNext = min freeSlots (length jobList)
       if enqueueNext == 0
       then reloop
       else do let nextJobs = take enqueueNext jobList
               mapM_ jobWrapper nextJobs
               reloop
    where
      jobWrapper :: Job -> IO ()
      jobWrapper job =
          do putStrLn $ "Starting work on " ++ (T.unpack $ unJobId jobId)
             atomically $ modifyTVar' (cs_processing ciState) $ \xs -> (jobId:xs)
             _ <- forkFinally realWork finallyHandler
             return ()
          where
            finallyHandler res =
                do atomically $ modifyTVar' (cs_processing ciState) (delete jobId)
                   case res of
                     Left (e :: SomeException) ->
                         putStrLn $ "Uncaught exception: " ++ show e
                     Right _ ->
                         return ()

            jobId = j_id job
            realWork =
                do (jobOk, jobLog) <-
                       jobHandler job `catch` \(e :: SomeException) ->
                           return (False, T.pack $ "Worker error: " ++ show e)
                   let jobSt = JobState jobId jobOk jobLog
                   (di_callbackHook dequeueIf) jobSt
      wait :: IO ()
      wait =
          threadDelay ((c_pollIntervalSec cfg) * 1000000)
      reloop :: IO ()
      reloop =
          do wait
             launchDequeue' dequeueIf ciState cfg jobHandler

test_launchDequeue =
    do st <- CiState <$> newTVarIO []
       let initialJobs = map mkJob [1..(100 :: Int)]
           jobSort = sortBy (comparing j_id)
       dummyTasks <- newTVarIO initialJobs
       finishedJobs <- newTVarIO []
       tid <- forkIO (launchDequeue' (dummyDequeueIf dummyTasks) st dummyCfg (jobHandler finishedJobs))
       finalState <-
           atomically $
             do openTasks <- readTVar dummyTasks
                unless (null openTasks) retry
                readTVar finishedJobs
       killThread tid
       assertEqual (jobSort initialJobs) (jobSort finalState)
    where
      dummyCfg =
          Config
          { c_pollUrl = PollUrl "none"
          , c_pollIntervalSec = 0
          , c_callbackUrl = CallbackUrl "non"
          , c_workers = 4
          }
      jobHandler finishedJobs job =
          do atomically $ modifyTVar' finishedJobs (job:)
             when (job == mkJob 50) $ fail "Oooops!"
             return (True, "Foo!")
      mkJob :: Int -> Job
      mkJob i =
          Job (JobId (T.pack $ show i)) (DownloadUrl "none")
      dummyDequeueIf dummyTasks =
          DequeueIf
          { di_pollJobs = atomically $ readTVar dummyTasks
          , di_callbackHook =
              \jobSt ->
                  atomically $
                  modifyTVar' dummyTasks (filter (\j -> j_id j /= (js_id jobSt)))
          }
