module Main where

import MAL
import System.Environment

credentialsFile :: IO FilePath
credentialsFile = do
    home <- getEnv "HOME"
    return $ home ++ "/.mal.conf"

main :: IO ()
main = do
    ecmd <- parseArgs <$> getArgs
    case ecmd of
      Left err -> error err
      Right cmd -> do
          creds <- credentialsFile >>= readCredentials
          verifyCredentials creds
          executeCommand creds cmd

