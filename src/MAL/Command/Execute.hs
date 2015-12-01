module MAL.Command.Execute where

import Control.Monad ( forM_ )
import qualified Data.Text as T

import MAL.API
import MAL.Credentials
import MAL.Command.Types
import MAL.Types
import Utils

executeCommand :: Credentials -> Command -> IO ()
executeCommand creds (List m uname) =
    case m of
      AnimeMode -> animeList creds uname >>= display "Animes:" . organize
      MangaMode -> mangaList creds uname >>= display "Mangas:" . organize


display :: String -> [(Maybe MyStatus, [T.Text])] -> IO ()
display p xs = do
    putStrLn p
    forM_ xs $ \(st, ys) -> do
        case st of
          Nothing -> return ()
          Just s -> do
              print s
              putStrLn $ T.unpack $ T.unlines ys
              newline

