{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAL.Types.Manga where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time
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

data Manga =
    Manga { _mangaName          :: T.Text           -- series_title
          , _mangaId            :: Id               -- series_mangadb_id
          , _mangaStatus        :: Maybe MyStatus   -- my_status
          , _mangaReadChapters  :: Int              -- my_read_chapters
          , _mangaTotalChapters :: Int              -- series_chapters
          , _mangaReadVolumes   :: Int              -- my_read_volumes
          , _mangaTotalVolumes  :: Int              -- series_volume
          , _mangaScore         :: Int              -- my_score
          , _mangaTags          :: T.Text           -- my_tags
          , _mangaType          :: Maybe MangaType  -- series_type
          , _mangaMyStartDate   :: Maybe UTCTime    -- my_start_date
          , _mangaMyEndDate     :: Maybe UTCTime    -- my_finish_date
          }
makeLenses ''Manga

incrReadChapters :: Manga -> Manga
incrReadChapters =
    over mangaReadChapters (+1)

incrReadVolumes :: Manga -> Manga
incrReadVolumes =
    over mangaReadVolumes (+1)

changeStatusM :: MyStatus -> Manga -> Manga
changeStatusM st =
    over mangaStatus (const $ Just st)

changeScoreM :: Int -> Manga -> Manga
changeScoreM s =
    over mangaScore (const s)

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
    , "my_start_date"
    , "my_finish_date"
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

instance ToXML Manga where
    toXml m =
        BS.pack $ unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          , "<entry>"
                          , "\t<chapter>" ++ show (_mangaReadChapters m) ++ "</chapter>"
                          , "\t<volume>" ++ show (_mangaReadVolumes m) ++ "</volume>"
                          , "\t<status>" ++ show (fromMyStatus $ _mangaStatus m) ++ "</status>"
                          , "\t<score>" ++ show (_mangaScore m) ++ "</score>"
                          , "\t<downloaded_chapters></downloaded_chapters>"
                          , "\t<times_reread></times_reread>"
                          , "\t<reread_value></reread_value>"
                          , "\t<date_start>" ++ maybe "0000-00-00" showTime (_mangaMyStartDate m) ++ "</date_start>"
                          , "\t<date_finish>" ++ maybe "0000-00-00" showTime (_mangaMyEndDate m) ++ "</date_finish>"
                          , "\t<priority>1</priority>"
                          , "\t<enable_discussion></enable_discussion>"
                          , "\t<enable_rereading></enable_rereading>"
                          , "\t<comments></comments>"
                          , "\t<scan_group></scan_group>"
                          , "\t<tags>" ++ T.unpack (_mangaTags m) ++ "</tags>"
                          , "\t<retail_volumes></retail_volumes>"
                          , "</entry>"
                          ]

