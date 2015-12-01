{-# LANGUAGE OverloadedStrings #-}

module MAL.API.List
    ( animeList
    , mangaList
    ) where

import Control.Applicative
import Control.Lens ( (^.) )
import qualified Data.Text as T
import Network.Wreq
import Text.XML
import Text.XML.Cursor

import MAL.Credentials
import MAL.Types
import Utils

listAttributes :: Credentials -> Name -> String -> [Name] -> IO [[T.Text]]
listAttributes creds ty uname attrs = do
    let url   = concat ["http://myanimelist.net/malappinfo.php?u=", uname, "&status=all&type=", T.unpack . nameLocalName $ ty ]
        opts' = opts creds
    r <- getWith opts' url
    let cur      = fromDocument . parseLBS_ def $ r ^. responseBody
        rootAxis = element "myanimelist" &/ element ty
    return $ map (\attr -> cur $| rootAxis &/ element attr &/ content) attrs

animeList :: Credentials -> String -> IO [Anime]
animeList creds uname = do
    res <- listAttributes creds "anime" uname animeAttributes
    let (titles:statuses:watched_eps:series_eps:scores:tags:types:[]) = res
        len          = length titles
        statuses'    = map (toMyStatus . textToInt) statuses
        watched_eps' = map textToInt watched_eps
        series_eps'  = map textToInt series_eps
        scores'      = map textToInt scores
        tags'        = extendList len "" tags
        types'       = map (toAnimeType . textToInt) types
    return $ getZipList $ Anime <$> ZipList titles
                                <*> ZipList statuses'
                                <*> ZipList watched_eps'
                                <*> ZipList series_eps'
                                <*> ZipList scores'
                                <*> ZipList tags'
                                <*> ZipList types'

mangaList :: Credentials -> String -> IO [Manga]
mangaList creds uname = do
    res <- listAttributes creds "manga" uname mangaAttributes
    let (names:statuses:read_chaps:series_chaps:read_vols:series_vols:scores:tags:types:[]) = res
        len           = length names
        statuses'     = map (toMyStatus . textToInt) statuses
        read_chaps'   = map textToInt read_chaps
        series_chaps' = map textToInt series_chaps
        read_vols'    = map textToInt read_vols
        series_vols'  = map textToInt $ extendList len "" series_vols
        scores'       = map textToInt scores
        tags'         = extendList len "" tags
        types'        = map (toMangaType . textToInt) types
    return $ getZipList $ Manga <$> ZipList names
                                <*> ZipList statuses'
                                <*> ZipList read_chaps'
                                <*> ZipList series_chaps'
                                <*> ZipList read_vols'
                                <*> ZipList series_vols'
                                <*> ZipList scores'
                                <*> ZipList tags'
                                <*> ZipList types'
