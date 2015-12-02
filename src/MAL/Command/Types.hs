module MAL.Command.Types where

import qualified Data.Text as T

data Mode = AnimeMode
          | MangaMode

type Username = String

data Command = List Mode (Maybe Username)
             | Inc Mode T.Text
             | IncVolume T.Text

