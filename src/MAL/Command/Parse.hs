module MAL.Command.Parse where

import MAL.Command.Types

type ParseError = String

parseArgs :: [String] -> Either ParseError Command
parseArgs ("anime":xs) = parseArgs' AnimeMode xs
parseArgs ("manga":xs) = parseArgs' MangaMode xs
parseArgs _            = Left $ "Parse error: first argument should be either anime or manga"

parseArgs' :: Mode -> [String] -> Either ParseError Command
parseArgs' m ("list":uname:_) = Right $ List m uname
parseArgs' _ xs               = Left $ "Parse error: command " ++ concat xs ++ " unknown"

