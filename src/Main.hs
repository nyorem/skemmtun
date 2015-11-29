{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens ( (&), (^.), (?~) )
import Control.Monad ( void )
import qualified Data.ByteString.Lazy as B
import Data.Function ( on )
import Data.List
import Data.Ord
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Network.HTTP.Client ( HttpException(..) )
import Network.Wreq
import Text.XML
import Text.XML.Cursor

credentialsFile :: FilePath
credentialsFile =
    "mal.txt"

-- (username, password)
type Credentials =
    (String, String)

-- Config file should have the following format:
-- username = foo
-- password = bar
readCredentials :: FilePath -> IO Credentials
readCredentials file = do
    contents <- Prelude.readFile file
    let ls = lines contents
        (u, p) = (head ls, head . tail $ ls)
        extract = head . tail . words . snd . span (/= '=')
    return $ (extract u, extract p)

-- Authentification credentials
opts :: Credentials -> Options
opts (username, password) =
    defaults & auth ?~ basicAuth (fromString username) (fromString password)

-- Check if the credentials are correct
verifyCredentials :: Credentials -> IO ()
verifyCredentials creds = do
    void $ getWith (opts creds) "http://myanimelist.net/api/account/verify_credentials.xml" `catch` handler
    where handler (StatusCodeException _ _ _) = error "Credentials verification failed!"

listAll :: Credentials -> Name -> String -> IO [(T.Text, Maybe MyStatus)]
listAll creds ty uname = do
    let url = concat ["http://myanimelist.net/malappinfo.php?u=", uname, "&status=all&type=", T.unpack $ nameLocalName ty ]
        opts' = opts creds
    r <- getWith opts' url
    let cur      = fromDocument $ parseLBS_ def $ r ^. responseBody
        titles   = cur $| element "myanimelist" &/ element ty &/ element "series_title" &/ content
        statuses'= cur $| element "myanimelist" &/ element ty &/ element "my_status" &/ content
        statuses = map (\s -> either (error "Error during status parsing") (toMyStatus . fst) $ T.decimal s) statuses'
    return $ sortBy (comparing snd) $ zip titles statuses

animeList :: Credentials -> String -> IO [(T.Text, Maybe MyStatus)]
animeList creds =
    listAll creds "anime"

mangaList :: Credentials -> String -> IO [(T.Text, Maybe MyStatus)]
mangaList creds =
    listAll creds "manga"

data MyStatus = Current
              | Completed
              | OnHold
              | Dropped
              | Planned
              deriving (Eq, Ord, Show)

toMyStatus :: Int -> Maybe MyStatus
toMyStatus 1 = Just Current
toMyStatus 2 = Just Completed
toMyStatus 3 = Just OnHold
toMyStatus 4 = Just Dropped
toMyStatus 6 = Just Planned
toMyStatus _ = Nothing

organize :: [(T.Text, Maybe MyStatus)] -> [(Maybe MyStatus, [T.Text])]
organize =
    map (\xs -> (snd . head $ xs, map fst xs)) . groupBy ((==) `on` snd)

main :: IO ()
main = do
    creds <- readCredentials credentialsFile
    verifyCredentials creds
    animeList creds "nyorem" >>= print . organize
    mangaList creds "nyorem" >>= print . organize

