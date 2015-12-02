module MAL.Command.Execute where

import Control.Monad ( forM_ )
import Data.List
import Data.Ord
import qualified Data.Text as T

import MAL.API
import MAL.Credentials
import MAL.Command.Types
import MAL.Types
import Pretty
import Utils

executeCommand :: Credentials -> Command -> IO ()
executeCommand creds (List m uname) =
    case m of
      AnimeMode -> animeList creds uname >>= displayAnimes . sortAndOrganizeBy _animeStatus
      MangaMode -> mangaList creds uname >>= displayMangas . sortAndOrganizeBy _mangaStatus

displayAnimes :: [(Maybe MyStatus, [Anime])] -> IO ()
displayAnimes xs =
    forM_ xs $ \(st, ys) -> do
        case st of
          Nothing -> return ()
          Just s -> do
              print s
              renderTable [ ColDesc center "#" center (show . _animeId)
                          , ColDesc left "Anime Name" left (T.unpack . _animeName)
                          , ColDesc center "Score" center (showInt . _animeScore)
                          , ColDesc center "Type" center (maybe "" show . _animeType)
                          , ColDesc center "Progress" center $ (\a -> show (_animeWatchedEpisodes a) ++ "/" ++ showInt (_animeTotalEpisodes a))
                          ] ys

displayMangas :: [(Maybe MyStatus, [Manga])] -> IO ()
displayMangas xs =
    forM_ xs $ \(st, ys) -> do
        case st of
          Nothing -> return ()
          Just s -> do
              print s
              renderTable [ ColDesc center "#" center (show . _mangaId)
                          , ColDesc left "Manga Title" left (T.unpack . _mangaName)
                          , ColDesc center "Score" center (showInt . _mangaScore)
                          , ColDesc center "Chapters" center $ (\m -> show (_mangaReadChapters m) ++ "/" ++ showInt (_mangaTotalChapters m))
                          , ColDesc center "Volumes" center $ (\m -> show (_mangaReadVolumes m) ++ "/" ++ showInt (_mangaTotalVolumes m))
                          , ColDesc center "Type" center (maybe "" show . _mangaType)
                          ] ys

sortAndOrganizeBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortAndOrganizeBy f =
    organizeBy f . sortBy (comparing f)

