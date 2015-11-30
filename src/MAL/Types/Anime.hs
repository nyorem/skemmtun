module MAL.Types.Anime where

import MAL.Types.Common

data AnimeType = TV
               | OVA
               | Movie
               | Special

-- TODO: add start/end dates
data Anime =
    Anime { _animeName :: String
          , _animeWatchedEpisodes :: Int
          , _animeTotalEpisodes :: Int
          , _animeScore :: Int
          , _animePriority :: Priority
          , _animeTags :: String
          , _animeType :: AnimeType
          }

