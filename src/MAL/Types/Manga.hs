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
               deriving Show

toMangaType :: Int -> Maybe MangaType
toMangaType 1 = Just Tankobon
toMangaType 2 = Just Novel
toMangaType _ = Nothing

-- TODO: add start/end dates
data Manga =
    Manga { _mangaName          :: T.Text           -- series_title
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
        untabs [ T.unpack $ _mangaName m
                , show $ _mangaScore m
                , show (_mangaReadChapters m) ++ "/" ++ show (_mangaTotalChapters m)
                , show (_mangaReadVolumes m) ++ "/" ++ show (_mangaTotalVolumes m)
                , maybe "" show $ _mangaType m
                ]

