{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MAL.Types.Anime where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Time
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

data Anime =
    Anime { _animeName            :: T.Text            -- series_title
          , _animeId              :: Id                -- series_animedb_id
          , _animeStatus          :: Maybe MyStatus    -- my_status
          , _animeWatchedEpisodes :: Int               -- my_watched_episodes
          , _animeTotalEpisodes   :: Int               -- series_episodes
          , _animeScore           :: Int               -- my_score
          , _animeTags            :: T.Text            -- my_tags
          , _animeType            :: Maybe AnimeType   -- series_type
          , _animeMyStartDate     :: Maybe UTCTime     -- my_start_date
          , _animeMyEndDate       :: Maybe UTCTime     -- my_finish_date
          }
makeLenses ''Anime

incrWatchedEpisodes :: Anime -> Anime
incrWatchedEpisodes =
    over animeWatchedEpisodes (+1)

changeStatusA :: MyStatus -> Anime -> Anime
changeStatusA st =
    over animeStatus (const $ Just st)

changeScoreA :: Int -> Anime -> Anime
changeScoreA s =
    over animeScore (const s)

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
    , "my_start_date"
    , "my_finish_date"
    ]

instance Show Anime where
    show a =
        untabs [ show $ _animeId a
               , T.unpack $ _animeName a
               , showInt $ _animeScore a
               , maybe "" show $ _animeType a
               , show (_animeWatchedEpisodes a) ++ "/" ++ showInt (_animeTotalEpisodes a)
               ]

instance ToXML Anime where
    toXml a =
        BS.pack $ unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          , "<entry>"
                          , "\t<episode>" ++ show (_animeWatchedEpisodes a) ++ "</episode>"
                          , "\t<status>" ++ show (fromMyStatus $ _animeStatus a) ++ "</status>"
                          , "\t<score>" ++ show (_animeScore a) ++ "</score>"
                          , "\t<downloaded_episodes></downloaded_episodes>"
                          , "\t<storage_type></storage_type>"
                          , "\t<storage_value></storage_value>"
                          , "\t<times_rewatched></times_rewatched>"
                          , "\t<rewatch_value></rewatch_value>"
                          , "\t<date_start>" ++ maybe "0000-00-00" showTime (_animeMyStartDate a) ++ "</date_start>"
                          , "\t<date_finish>" ++ maybe "0000-00-00" showTime (_animeMyEndDate a) ++ "</date_finish>"
                          , "\t<priority>1</priority>"
                          , "\t<enable_discussion></enable_discussion>"
                          , "\t<enable_rewatching></enable_rewatching>"
                          , "\t<comments></comments>"
                          , "\t<fansub_group></fansub_group>"
                          , "\t<tags>" ++ T.unpack (_animeTags a) ++ "</tags>"
                          , "</entry>"
                          ]

