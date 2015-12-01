module MAL.Command.Execute where

import Control.Monad ( forM_ )
import Data.List
import Data.Ord

import MAL.API
import MAL.Credentials
import MAL.Command.Types
import MAL.Types
import Utils

executeCommand :: Credentials -> Command -> IO ()
executeCommand creds (List m uname) =
    case m of
      AnimeMode -> animeList creds uname >>= display "Animes:" . sortAndOrganizeBy _animeStatus
      MangaMode -> mangaList creds uname >>= display "Mangas:" . sortAndOrganizeBy _mangaStatus

display :: (Show a, Show b) => String -> [(Maybe b, [a])] -> IO ()
display p xs = do
    putStrLn p
    forM_ xs $ \(st, ys) -> do
        case st of
          Nothing -> return ()
          Just s -> do
              print s
              putStrLn $ unlines $ map (("- " ++) . show) ys
              newline

sortAndOrganizeBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortAndOrganizeBy f =
    organizeBy f . sortBy (comparing f)

