{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Cook.Ci.Dequeue where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad
import Cook.Ci.Types
import Data.Aeson
import Data.List
import Network.Wreq hiding (delete)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

downloadBinary :: DownloadUrl -> IO BSL.ByteString
downloadBinary (DownloadUrl url) =
    do r <- get (T.unpack url)
       return (r ^. responseBody)

launchDequeue :: CiState -> Config -> (Job -> IO (Bool, T.Text)) -> IO ()
launchDequeue ciState cfg jobHandler =
    do r <- asJSON =<< get (T.unpack $ unPollUrl $ c_pollUrl cfg)
       currentJobs <- atomically $ readTVar (cs_processing ciState)
       let jobListOrig = r ^. responseBody
           jobList = filter (\j -> not $ j_id j `elem` currentJobs) jobListOrig
           freeSlots = (c_workers cfg) - (length currentJobs)
           enqueueNext = min freeSlots (length jobList)
       if enqueueNext == 0
       then reloop
       else do let nextJobs = take enqueueNext jobList
               mapM_ (forkIO . jobWrapper) nextJobs
               reloop
    where
      jobWrapper :: Job -> IO ()
      jobWrapper job =
          do putStrLn $ "Starting work on " ++ (T.unpack $ unJobId jobId)
             realWork `finally` (atomically $ modifyTVar' (cs_processing ciState) (delete jobId))
          where
            jobId = j_id job
            realWork =
                do atomically $ modifyTVar' (cs_processing ciState) $ \xs -> (jobId:xs)
                   (jobOk, jobLog) <-
                       jobHandler job `catch` \(e :: SomeException) ->
                           return (False, T.pack $ "Worker error: " ++ show e)
                   let jobSt = JobState jobId jobOk jobLog
                   r <- post (T.unpack $ unCallbackUrl $ c_callbackUrl cfg) (toJSON jobSt)
                   let respCode = r ^. responseStatus . statusCode
                   when (respCode /= 200) $
                        putStrLn "Callback-URL didn't respond with status 200!"
      wait :: IO ()
      wait =
          threadDelay ((c_pollIntervalSec cfg) * 1000000)
      reloop :: IO ()
      reloop =
          do wait
             launchDequeue ciState cfg jobHandler
