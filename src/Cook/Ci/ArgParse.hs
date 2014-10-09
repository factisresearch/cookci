module Cook.Ci.ArgParse (parseArgs) where

import Cook.Ci.Types

import Options.Applicative
import qualified Data.Text as T

parseArgs :: Parser Config
parseArgs =
    Config <$> pollUrlP <*> pollIntervalP <*> callbackUrlP <*> workerP <*> taskTimeoutP

callbackUrlP :: Parser CallbackUrl
callbackUrlP =
    (CallbackUrl . T.pack) <$>
    (strOption $
    long "callback-url" <>
    short 'c' <>
    metavar "URL" <>
    help ("url to be called when a build terminates (failure/success). A POST request "
          ++ "will be sent to this url in the following format:\n"
          ++ "{'id':'someId', 'success':true/false, 'log':'foobarbaz'}"))

pollUrlP :: Parser PollUrl
pollUrlP =
    (PollUrl . T.pack) <$>
    (strOption $
    long "poll-url" <>
    short 'p' <>
    metavar "URL" <>
    help ("where to look for new builds? The url should respond to GET requests in "
          ++ "the following format:\n"
          ++ "[{'id':'someId', 'download':'http://somfile.tar.gz'}]"))

pollIntervalP :: Parser Int
pollIntervalP =
    option $
    long "poll-interval" <>
    short 's' <>
    value 60 <>
    metavar "SECONDS" <>
    help "how often in seconds should the poll url be polled for new builds"

taskTimeoutP :: Parser Int
taskTimeoutP =
    option $
    long "task-timeout" <>
    short 't' <>
    value 900 <>
    metavar "SECONDS" <>
    help "after how many seconds should a build task be canceled and marked as failed"

workerP :: Parser Int
workerP =
    option $
    long "worker-threads" <>
    short 'n' <>
    value 4 <>
    metavar "THREADS" <>
    help "maximum amount of parallel builds"
