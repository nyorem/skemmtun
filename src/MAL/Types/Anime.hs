{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAL.Types.Anime where

import Control.Lens
import qualified Data.Text as T
import Text.XML

import MAL.Types.Common
import Utils

data AnimeType = TV
               | OVA
               | Movie
               | Special
               | ONA
               | Music
               deriving Show

toAnimeType :: Int -> Maybe AnimeType
toAnimeType 1 = Just TV
toAnimeType 2 = Just OVA
toAnimeType 3 = Just Movie
toAnimeType 4 = Just Special
toAnimeType 5 = Just ONA
toAnimeType 6 = Just Music
toAnimeType _ = Nothing

-- TODO: add start/end dates
data Anime =
    Anime { _animeName            :: T.Text            -- series_title
          , _animeId              :: Int               -- series_animedb_id
          , _animeStatus          :: Maybe MyStatus    -- my_status
          , _animeWatchedEpisodes :: Int               -- my_watched_episodes
          , _animeTotalEpisodes   :: Int               -- series_episodes
          , _animeScore           :: Int               -- my_score
          , _animeTags            :: T.Text            -- my_tags
          , _animeType            :: Maybe AnimeType   -- series_type
          }
makeLenses ''Anime

animeAttributes :: [Name]
animeAttributes =
    [ "series_title"
    , "series_animedb_id"
    , "my_status"
    , "my_watched_episodes"
    , "series_episodes"
    , "my_score"
    , "my_tags"
    , "series_type"
    ]

instance Show Anime where
    show a =
        untabs [ show $ _animeId a
               , T.unpack $ _animeName a
               , show $ _animeScore a
               , maybe "" show $ _animeType a
               , show (_animeWatchedEpisodes a) ++ "/" ++ show (_animeTotalEpisodes a)
               ]

