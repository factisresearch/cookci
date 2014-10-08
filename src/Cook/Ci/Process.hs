{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Cook.Ci.Process
    ( runInteractiveProcess
    )
where

import Control.Concurrent
import Control.Monad (when)
import System.Exit (ExitCode(..))
import System.IO
import qualified Control.Exception as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified System.Process as P

runInteractiveProcess :: String
                      -> [String]
                      -> Maybe FilePath
                      -> (BS.ByteString -> IO ())
                      -> BSL.ByteString       -- ^ standard input
                      -> IO ( ExitCode
                            , BSL.ByteString
                            , BSL.ByteString) -- ^ (exitcode, stdout, stderr)
runInteractiveProcess process args workDir errorLogger input =
    C.catch (C.bracketOnError startProcess closeProcess withProcess)
    (\(exc::C.IOException) ->
         do errorLogger (BSC.pack $ msg ++ " failed: " ++ show exc)
            C.throwIO exc)
    where
      msg = ("running process " ++ process)
      startProcess = P.runInteractiveProcess process args workDir Nothing
      closeProcess (_inh, _outh, _errh, pid) =
          do P.terminateProcess pid
             _ <- P.waitForProcess pid
             return ()
      withProcess (inh, outh, errh, pid) =
          do hSetBinaryMode inh True
             hSetBinaryMode outh True
             hSetBinaryMode errh True
             getOut <- startHandleReader outh
             getErr <- startHandleReader errh
             threadDelay 100000 --- 100 ms
             -- now write and flush any input
             when (not (BSL.null input)) $
                  do errorLogger (BSC.pack $ "Writing " ++ show (BSL.length input) ++ " to stdin of process.")
                     BSL.hPutStr inh input
                     errorLogger ("Flushing stdin of process.")
                     hFlush inh
             errorLogger ("Closing stdin of process.")
             hClose inh -- done with stdin
             errorLogger ("Done closing stdin of process.")
             -- wait on the output
             out <- getOut
             errorLogger ("Done waiting for out")
             err <- getErr
             errorLogger ("Done waiting for err")
             ecode <- P.waitForProcess pid
             return (ecode, out, err)
      startHandleReader h =
          do resMVar <- newEmptyMVar
             _ <- forkIO $
                  try $ BS.hGetContents h >>= putMVar resMVar . (BSL.fromChunks . (:[]))
             return (takeMVar resMVar)
      try :: IO () -> IO ()
      try action =
          C.catch action $
               \(exc :: C.IOException) ->
                   do errorLogger (BSC.pack $ msg ++ ": " ++ show exc)
                      return ()
