{-# LANGUAGE TemplateHaskell #-}

module MAL.Types.Anime where

import Control.Lens
import MAL.Types.Common

data AnimeType = TV
               | OVA
               | Movie
               | Special
               deriving Show

toAnimeType :: Int -> Maybe AnimeType
toAnimeType 1 = Just TV
toAnimeType 2 = Just OVA
toAnimeType 3 = Just Movie
toAnimeType 4 = Just Special
toAnimeType _ = Nothing

-- TODO: add start/end dates
data Anime =
    Anime { _animeName            :: String            -- series_title
          , _animeStatus          :: MyStatus          -- my_status
          , _animeWatchedEpisodes :: Int               -- my_watched_episodes
          , _animeTotalEpisodes   :: Int               -- series_episode
          , _animeScore           :: Int               -- my_score
          , _animeTags            :: String            -- my_tags
          , _animeType            :: AnimeType         -- series_type
          }
makeLenses ''Anime

instance Show Anime where
    show a =
        unwords [ _animeName a
                , show $ _animeScore a
                , show $ _animeType a
                , show (_animeWatchedEpisodes a) ++ "/" ++ show (_animeTotalEpisodes a)
                ]
