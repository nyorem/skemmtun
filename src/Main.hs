module Main where

import Control.Monad ( forM_ )
import qualified Data.Text as T

import MAL.API
import MAL.Credentials
import MAL.Types
import Utils

credentialsFile :: FilePath
credentialsFile =
    "mal.txt"

display :: String -> [(Maybe MyStatus, [T.Text])] -> IO ()
display p xs = do
    putStrLn p
    forM_ xs $ \(st, ys) -> do
        case st of
          Nothing -> return ()
          Just s -> do
              print s
              putStrLn $ T.unpack $ T.unlines ys
              newline

main :: IO ()
main = do
    creds <- readCredentials credentialsFile
    verifyCredentials creds
    animeList creds "nyorem" >>= display "Animes:" . organize
    mangaList creds "nyorem" >>= display "Mangas:" . organize

