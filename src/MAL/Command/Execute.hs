{-# LANGUAGE OverloadedStrings #-}

module MAL.Command.Execute where

import Control.Monad ( forM_ )
import qualified Data.Text as T

import MAL.API
import MAL.Credentials
import MAL.Command.Types
import MAL.Types
import Pretty
import Utils

executeCommand :: Credentials -> Command -> IO ()
executeCommand _ Help = do
    mapM_ putStrLn $ [ "Usage: mal mode command"
                     , ""
                     , "- Available modes:"
                     , "\tanime"
                     , "\tmanaga"
                     , "- Available commands:"
                     , "\tlist"
                     , "\tinc: increment chapter / episode number"
                     , "\tincv: increment volume number"
                     , "\tset: change status"
                     ]

executeCommand creds (List m st muname) = do
    let uname = maybe (fst creds) id muname
    case m of
      AnimeMode -> animeList creds uname >>= displayAnimes st . sortAndOrganizeBy _animeStatus
      MangaMode -> mangaList creds uname >>= displayMangas st . sortAndOrganizeBy _mangaStatus

executeCommand creds (Inc m name) =
    case m of
      AnimeMode -> update creds name animeList _animeName _animeId "Anime" incrWatchedEpisodes
      MangaMode -> update creds name mangaList _mangaName _mangaId "Manga" incrReadChapters

executeCommand creds (IncVolume name) =
    update creds name mangaList _mangaName _mangaId "Manga" incrReadVolumes

executeCommand creds (Set m name cmd) =
    case cmd of
      SetStatus st ->
          case m of
            AnimeMode -> update creds name animeList _animeName _animeId "Anime" $ changeStatusA st
            MangaMode -> update creds name mangaList _mangaName _mangaId "Anime" $ changeStatusM st

displayAnimes :: Maybe MyStatus -> [(Maybe MyStatus, [Anime])] -> IO ()
displayAnimes st xs = do
    let as =
            case st of
              Nothing -> xs
              _        -> filter (\(s, _) -> s == st) xs
    forM_ as $ \(st', ys) -> do
        case st' of
          Nothing -> return ()
          Just s -> do
              print s
              renderTable [ ColDesc center "#" center (show . _animeId)
                          , ColDesc left "Anime Name" left (T.unpack . _animeName)
                          , ColDesc center "Score" center (showInt . _animeScore)
                          , ColDesc center "Type" center (maybe "" show . _animeType)
                          , ColDesc center "Progress" center $ (\a -> show (_animeWatchedEpisodes a) ++ "/" ++ showInt (_animeTotalEpisodes a))
                          ] ys

displayMangas :: Maybe MyStatus -> [(Maybe MyStatus, [Manga])] -> IO ()
displayMangas st xs = do
    let ms =
            case st of
              Nothing -> xs
              _        -> filter (\(s, _) -> s == st) xs
    forM_ ms $ \(st', ys) -> do
        case st' of
          Nothing -> return ()
          Just s -> do
              print s
              renderTable [ ColDesc center "#" center (show . _mangaId)
                          , ColDesc left "Manga Title" left (T.unpack . _mangaName)
                          , ColDesc center "Score" center (showInt . _mangaScore)
                          , ColDesc center "Chapters" center $ (\m -> show (_mangaReadChapters m) ++ "/" ++ showInt (_mangaTotalChapters m))
                          , ColDesc center "Volumes" center $ (\m -> show (_mangaReadVolumes m) ++ "/" ++ showInt (_mangaTotalVolumes m))
                          , ColDesc center "Type" center (maybe "" show . _mangaType)
                          ] ys

