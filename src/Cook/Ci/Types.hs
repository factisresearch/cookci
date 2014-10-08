{-# LANGUAGE OverloadedStrings #-}
module Cook.Ci.Types where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import qualified Data.Text as T

data CiState
   = CiState
   { cs_processing :: TVar [JobId]
   }

newtype PollUrl
    = PollUrl { unPollUrl :: T.Text }

newtype CallbackUrl
    = CallbackUrl { unCallbackUrl :: T.Text }

data Config
   = Config
   { c_pollUrl :: PollUrl
   , c_pollIntervalSec :: Int
   , c_callbackUrl :: CallbackUrl
   , c_workers :: Int
   }

newtype JobId
    = JobId { unJobId :: T.Text }
    deriving (Show, Eq)

newtype DownloadUrl
    = DownloadUrl { unDownloadUrl :: T.Text }

data Job
   = Job
   { j_id :: JobId
   , j_downloadUrl :: DownloadUrl
   }

data JobState
   = JobState
   { js_id :: JobId
   , js_success :: Bool
   , js_log :: T.Text
   }

instance FromJSON Job where
    parseJSON (Object v) =
        Job <$>
        (JobId <$> v .: "id") <*>
        (DownloadUrl <$> v .: "download")
    parseJSON _ = mzero

instance ToJSON JobState where
    toJSON (JobState (JobId jid) success logV) =
        object
        [ "id" .= jid
        , "success" .= success
        , "log" .= logV
        ]
