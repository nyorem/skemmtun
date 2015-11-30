module MAL.Types.Manga where

import MAL.Types.Common

data MangaType = Tankobon
               | Novel

-- TODO: add start/end dates
data Manga =
    Manga { _mangaName          :: String
          , _mangaReadChapters  :: Int
          , _mangaTotalChapters :: Int
          , _mangaReadVolumes   :: Int
          , _mangaTotalVolumes  :: Int
          , _mangaScore         :: Int
          , _mangaPriority      :: Priority
          , _mangaTags          :: String
          , _mangaType          :: MangaType
          }

