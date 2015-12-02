{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAL.Types.Manga where

import Control.Lens
import qualified Data.Text as T
import Text.XML

import MAL.Types.Common
import Utils

data MangaType = Tankobon
               | Novel
               | OneShot
               | Doujin
               | Manhwa
               | Manhua
               deriving Show

toMangaType :: Int -> Maybe MangaType
toMangaType 1 = Just Tankobon
toMangaType 2 = Just Novel
toMangaType 3 = Just OneShot
toMangaType 4 = Just Doujin
toMangaType 5 = Just Manhwa
toMangaType 6 = Just Manhua
toMangaType _ = Nothing

-- TODO: add start/end dates
data Manga =
    Manga { _mangaName          :: T.Text           -- series_title
          , _mangaId            :: Int              -- series_mangadb_id
          , _mangaStatus        :: Maybe MyStatus   -- my_status
          , _mangaReadChapters  :: Int              -- my_read_chapters
          , _mangaTotalChapters :: Int              -- series_chapters
          , _mangaReadVolumes   :: Int              -- my_read_volumes
          , _mangaTotalVolumes  :: Int              -- series_volume
          , _mangaScore         :: Int              -- my_score
          , _mangaTags          :: T.Text           -- my_tags
          , _mangaType          :: Maybe MangaType  -- series_type
          }
makeLenses ''Manga

mangaAttributes :: [Name]
mangaAttributes =
    [ "series_title"
    , "series_mangadb_id"
    , "my_status"
    , "my_read_chapters"
    , "series_chapters"
    , "my_read_volumes"
    , "series_volumes"
    , "my_score"
    , "my_tags"
    , "series_type"
    ]

instance Show Manga where
    show m =
        untabs [ show $ _mangaId m
               , T.unpack $ _mangaName m
               , showInt $ _mangaScore m
               , show (_mangaReadChapters m) ++ "/" ++ showInt (_mangaTotalChapters m)
               , show (_mangaReadVolumes m) ++ "/" ++ showInt (_mangaTotalVolumes m)
               , maybe "" show $ _mangaType m
               ]

