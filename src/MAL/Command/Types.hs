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
             | SetWatchedEpisodes Int String
             | SetReadChapters Int String
             | SetReadVolumes Int String

data CommandSet = SetStatus MyStatus
                | SetScore Int

