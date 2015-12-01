{-# LANGUAGE TemplateHaskell #-}

module MAL.Types.Manga where

import Control.Lens
import MAL.Types.Common

data MangaType = Tankobon
               | Novel
               deriving Show

toMangaType :: Int -> Maybe MangaType
toMangaType 1 = Just Tankobon
toMangaType 2 = Just Novel
toMangaType _ = Nothing

-- TODO: add start/end dates
data Manga =
    Manga { _mangaName          :: String           -- series_title
          , _mangaStatus        :: MyStatus         -- my_status
          , _mangaReadChapters  :: Int              -- my_read_chapters
          , _mangaTotalChapters :: Int              -- series_chapters
          , _mangaReadVolumes   :: Int              -- my_read_volumes
          , _mangaTotalVolumes  :: Int              -- series_volume
          , _mangaScore         :: Int              -- my_score
          , _mangaTags          :: String           -- my_tags
          , _mangaType          :: MangaType        -- series_type
          }
makeLenses ''Manga

instance Show Manga where
    show m =
        unwords [ _mangaName m
                , show $ _mangaScore m
                , show (_mangaReadChapters m) ++ "/" ++ show (_mangaTotalChapters m)
                , show (_mangaReadVolumes m) ++ "/" ++ show (_mangaTotalVolumes m)
                , show $ _mangaType m
                ]
