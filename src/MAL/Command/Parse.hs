module MAL.Command.Parse where

import MAL.Command.Types
import MAL.Types.Common
import Utils

type ParseError = String

parseArgs :: [String] -> Either ParseError Command
parseArgs ("-a":xs) = parseArgs' AnimeMode xs
parseArgs ("-m":xs) = parseArgs' MangaMode xs
parseArgs (m:_)     = Left $ "Parse error: mode " ++ m ++ " unknown"
parseArgs _         = Right Help

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
                    st'     -> (st', Just uname, Right undefined)
              _             -> (Nothing, Nothing, Left $ "Parse error: unknown arguments " ++ concatSpaces xs)
    in
        case err of
          Left _ -> err
          _      -> Right $ List m s u

parseArgs' m ("inc":name) = Right $ Inc m $ concatSpaces name

parseArgs' MangaMode ("incv":name) = Right $ IncVolume $ concatSpaces name

parseArgs' MangaMode ("set":"--read":n:name) =
    case maybeRead n of
      Nothing -> Left $ "Parse error: " ++ n ++ " is not a valid number."
      Just n' -> Right $ SetReadChapters n' $ concatSpaces name

parseArgs' MangaMode ("set":"--readv":n:name) =
    case maybeRead n of
      Nothing -> Left $ "Parse error: " ++ n ++ " is not a valid number."
      Just n' -> Right $ SetReadVolumes n' $ concatSpaces name

parseArgs' AnimeMode ("set":"--watched":n:name) =
    case maybeRead n of
      Nothing -> Left $ "Parse error: " ++ n ++ " is not a valid number."
      Just n' -> Right $ SetWatchedEpisodes n' $ concatSpaces name

parseArgs' m  ("set":xs) =
    case xs of
      ("--status":st:name) ->
          case parseMyStatus st of
            Nothing -> Left $ "Parse error: status " ++ st ++ "unknown."
            Just s  -> Right $ Set m (concatSpaces name) (SetStatus s)
      ("--score":s:name) ->
          case maybeRead s of
            Nothing -> Left $ "Parse error: " ++ s ++ " is not a valid score."
            Just s' -> if s' < 0 || s' > 10 then
                            Left $ "Error: the score must be between 0 and 10."
                        else
                            Right $ Set m (concatSpaces name) (SetScore s')
      _                  -> Left $ "Parse error: unkown subcommand " ++ concatSpaces xs

parseArgs' m ("search":xs) =
    Right $ Search m $ concatSpaces xs

parseArgs' m ("delete":mn:_) =
    case maybeRead mn of
      Nothing -> Left $ error $ "Parse error: " ++ mn ++ " is not a valid Id."
      Just n  -> Right $ Delete m n

parseArgs' _ xs = Left $ "Parse error: command " ++ concatSpaces xs ++ " unknown"

