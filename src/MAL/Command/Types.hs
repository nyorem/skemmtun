module MAL.Command.Types where

import MAL.Types

data Mode = AnimeMode
          | MangaMode
          deriving (Eq)

instance Show Mode where
    show AnimeMode = "anime"
    show MangaMode = "manga"

type Username = String

data Command = Help
             | List Mode (Maybe MyStatus) (Maybe Username)
             | Inc Mode String
             | IncVolume String
             | Set Mode String CommandSet
             | SetWatchedEpisodes Int String
             | SetReadChapters Int String
             | SetReadVolumes Int String
             | Search Mode String

data CommandSet = SetStatus MyStatus
                | SetScore Int

