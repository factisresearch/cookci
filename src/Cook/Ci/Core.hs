{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Cook.Ci.Core (runCore) where

import Cook.Ci.Dequeue
import Cook.Ci.Process
import Cook.Ci.Types

import System.Exit
import System.IO
import System.IO.Temp
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Process as P

runCore :: Config -> IO ()
runCore cfg =
    launchDequeue cfg $ \(Job _ downloadUrl) ->
        withSystemTempDirectory "cookciXXXX" $ \tempDir ->
        withSystemTempFile "cooktarXXXX" $ \tarFp tarHdl ->
           do hClose tarHdl
              tarBsl <- downloadBinary downloadUrl
              BSL.writeFile tarFp tarBsl
              ec <- P.rawSystem "tar" ["-x", "-f", tarFp, "-C", tempDir]
              if ec == ExitSuccess
              then launchBuild tempDir
              else return (False, "Failed to unpack the provided .tar.gz file!")

launchBuild :: FilePath -> IO (Bool, T.Text)
launchBuild fp =
    do (ec, stdOut, stdErr) <-
           runInteractiveProcess "/bin/bash" ["build.sh"] (Just fp) (putStrLn . BSC.unpack) ""
       let conv = T.decodeUtf8 . BSL.toStrict
           out = T.concat [conv stdOut, "\n", conv stdErr]
       return (ec == ExitSuccess, out)
