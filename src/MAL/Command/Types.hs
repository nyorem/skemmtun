module MAL.Command.Types where

import MAL.Types

data Mode = AnimeMode
          | MangaMode

type Username = String

data Command = Help
             | List Mode (Maybe MyStatus) (Maybe Username)
             | Inc Mode String
             | IncVolume String
             | Set Mode String CommandSet

data CommandSet = SetStatus MyStatus

