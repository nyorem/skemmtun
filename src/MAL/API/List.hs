{-# LANGUAGE OverloadedStrings #-}

module MAL.API.List where

import Control.Lens ( (^.) )
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Network.Wreq
import Text.XML
import Text.XML.Cursor

import MAL.Credentials
import MAL.Types

listAll :: Credentials -> Name -> String -> IO [(T.Text, Maybe MyStatus)]
listAll creds ty uname = do
    let url = concat ["http://myanimelist.net/malappinfo.php?u=", uname, "&status=all&type=", T.unpack . nameLocalName $ ty ]
        opts' = opts creds
    r <- getWith opts' url
    let cur      = fromDocument . parseLBS_ def $ r ^. responseBody
        axis     = element "myanimelist" &/ element ty
        titles   = cur $| axis &/ element "series_title" &/ content
        statuses'= cur $| axis &/ element "my_status" &/ content
        statuses = map (\s -> either (error "Error during status parsing") (toMyStatus . fst) $ T.decimal s) statuses'
    return $ sortBy (comparing snd) $ zip titles statuses

animeList :: Credentials -> String -> IO [(T.Text, Maybe MyStatus)]
animeList creds =
    listAll creds "anime"

mangaList :: Credentials -> String -> IO [(T.Text, Maybe MyStatus)]
mangaList creds =
    listAll creds "manga"

