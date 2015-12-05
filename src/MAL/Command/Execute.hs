{-# LANGUAGE OverloadedStrings #-}

module MAL.Command.Execute where

import qualified Data.Text as T

import MAL.API
import MAL.Credentials
import MAL.Command.Types
import MAL.Types
import Utils

executeCommand :: Credentials -> Command -> IO ()
executeCommand _ Help = do
    mapM_ putStrLn $ [ "Usage: mal mode command"
                     , ""
                     , "- Available modes:"
                     , "\t-a: anime"
                     , "\t-m: tmanga"
                     , ""
                     , "- Available commands:"
                     , "\tlist"
                     , "\tinc: increment chapter / episode number"
                     , "\tincv: increment volume number (only manga mode)"
                     , "\tset: --status, --score, --read, --readv, --watched"
                     , "\tsearch: search and add animes / mangas"
                     , "\tdelete: delete animes / mangas"
                     , ""
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
            MangaMode -> update creds name mangaList _mangaName _mangaId "Manga" $ changeStatusM st
      SetScore s ->
          case m of
            AnimeMode -> update creds name animeList _animeName _animeId "Anime" $ changeScoreA s
            MangaMode -> update creds name mangaList _mangaName _mangaId "Manga" $ changeScoreM s

executeCommand creds (SetWatchedEpisodes n name) =
    update creds name animeList _animeName _animeId "Anime" $ changeWatchedEpisodes n

executeCommand creds (SetReadChapters n name) =
    update creds name mangaList _mangaName _mangaId "Manga" $ changeReadChapters n

executeCommand creds (SetReadVolumes n name) =
    update creds name mangaList _mangaName _mangaId "Manga" $ changeReadVolumes n

executeCommand creds (Search m req) = do
    res <- searchFor creds (show m) req
    mn <- prompt' T.unpack res
    case mn of
      Nothing -> putStrLn "Not added!"
      Just n -> do
          putStrLn $ "Would you like to add it to your collection? (o/n)"
          shouldAdd <- getChar
          case shouldAdd of
            'o' -> do
                let addf = if m == AnimeMode then addAnime else addManga
                addf creds n
                putStrLn "Added!"
            _   -> putStrLn "Not added!"

executeCommand creds (Delete m n) = do
    putStrLn $ "Are you sure you want to delete " ++ show n ++ "? (o/n)"
    c <- getChar
    case c of
      'o' -> do
          let delf = if m == AnimeMode then deleteAnime else deleteManga
          delf creds n
          putStrLn "Deleted!"
      _  -> putStrLn "Not deleted!"

