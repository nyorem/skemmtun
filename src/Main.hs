module Main where

import MAL
import System.Environment

credentialsFile :: FilePath
credentialsFile =
    "mal.txt"

main :: IO ()
main = do
    ecmd <- parseArgs <$> getArgs
    case ecmd of
      Left err -> error err
      Right cmd -> do
          creds <- readCredentials credentialsFile
          verifyCredentials creds
          executeCommand creds cmd

