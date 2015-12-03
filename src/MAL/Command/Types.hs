module MAL.Command.Types where

import MAL.Types

import qualified Data.Text as T

data Mode = AnimeMode
          | MangaMode

type Username = String

data Command = Help
             | List Mode (Maybe MyStatus) (Maybe Username)
             | Inc Mode T.Text
             | IncVolume T.Text

