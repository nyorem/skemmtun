{-# LANGUAGE OverloadedStrings #-}

module MAL.Command.Execute where

import Control.Monad
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
                     , "\tanime"
                     , "\tmanaga"
                     , "- Available commands:"
                     , "\tlist"
                     , "\tinc: increment chapter / episode number"
                     , "\tincv: increment volume number"
                     , "\tset: change status, score"
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
    putStrLn $ "Multiple results: choose one of the following (q to quit)"

    forM_ res $ \(i, name) -> do
        putStrLn $ show i ++ ") " ++ T.unpack name

    c <- getLine
    case c of
      "q" -> return ()
      mid ->
          case maybeRead mid of
            Nothing -> putStrLn "Not added!"
            Just n -> do
                -- TODO: add the chosen anime / manga
                let n' = n :: Id
                undefined
                putStrLn "Added!"

