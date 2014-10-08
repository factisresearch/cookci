module Main where

import Cook.Ci.ArgParse
import Cook.Ci.Core

import Options.Applicative

main :: IO ()
main =
    execParser opts >>= runCore
    where
      opts = info (helper <*> parseArgs)
             ( fullDesc
             <> progDesc "A simple CI tool for dockercook"
             )
