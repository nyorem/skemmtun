module MAL.Command.Parse where

import Data.List
import qualified Data.Text as T

import MAL.Command.Types
import MAL.Types.Common

type ParseError = String

parseArgs :: [String] -> Either ParseError Command
parseArgs ("anime":xs) = parseArgs' AnimeMode xs
parseArgs ("manga":xs) = parseArgs' MangaMode xs
parseArgs (m:_)        = Left $ "Parse error: mode " ++ m ++ " unknown"
parseArgs _            = Right Help

-- TODO; remove ugly undefined (Either monad?)
parseArgs' :: Mode -> [String] -> Either ParseError Command
parseArgs' m ("list":xs) =
    let (s, u, err) =
            case xs of
              []            -> (Nothing, Nothing, Right undefined)
              (x:[])        ->
                  case parseMyStatus x of
                    Nothing -> (Nothing, Just x, Right undefined)
                    st      -> (st, Nothing, Right undefined)
              (st:uname:[]) ->
                  case parseMyStatus st of
                    Nothing -> (Nothing, Nothing, Left $ "Parse error: status " ++ st ++ " unknown")
                    st' -> (st', Just uname, Right undefined)
              _             -> (Nothing, Nothing, Left $ "Parse error: unknown arguments " ++ intercalate " " xs)
    in
        case err of
          Left _ -> err
          _       -> Right $ List m s u

parseArgs' m ("inc":name) = Right $ Inc m $ T.pack $ intercalate " " name

parseArgs' MangaMode ("incv":name) = Right $ IncVolume $ T.pack $ intercalate " " name

parseArgs' _ xs               = Left $ "Parse error: command " ++ intercalate " " xs ++ " unknown"

