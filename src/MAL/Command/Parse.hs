module MAL.Command.Parse where

import Data.List
import qualified Data.Text as T

import MAL.Command.Types

type ParseError = String

parseArgs :: [String] -> Either ParseError Command
parseArgs ("anime":xs) = parseArgs' AnimeMode xs
parseArgs ("manga":xs) = parseArgs' MangaMode xs
parseArgs _            = Left $ "Parse error: first argument should be either anime or manga"

parseArgs' :: Mode -> [String] -> Either ParseError Command
parseArgs' m ("list":xs) =
    Right $ List m $
        if null xs then Nothing else Just $ head xs
parseArgs' m ("inc":name) = Right $ Inc m $ T.pack $ intercalate " " name
parseArgs' MangaMode ("incv":name) = Right $ IncVolume $ T.pack $ intercalate " " name
parseArgs' _ xs               = Left $ "Parse error: command " ++ concat xs ++ " unknown"

