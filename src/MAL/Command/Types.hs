module MAL.Command.Types where

data Mode = AnimeMode
          | MangaMode

type Username = String

data Command = List Mode Username

