module Utils where

import Control.Monad
import Data.Function
import Data.Char
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Time

-- LIST

organizeBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
organizeBy f =
    map (\xs -> (f . head $ xs, xs)) . groupBy ((==) `on` f)

sortAndOrganizeBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortAndOrganizeBy f =
    organizeBy f . sortBy (comparing f)

findPrefixes :: (Eq b) => Int -> (a -> [b]) -> [b] -> [a] -> [a]
findPrefixes n f x =
    take n . filter (\y -> x `isPrefixOf` (f y))

-- CHAR

toLowers :: String -> String
toLowers =
    map toLower

-- PRINT

newline :: IO ()
newline =
    putStrLn ""

textToInt :: T.Text -> Int
textToInt =
    either (error "Error during Text parsing") fst . T.decimal

extendList :: Int -> a -> [a] -> [a]
extendList n z xs
    | len > n   = take len xs
    | len < n   = xs ++ replicate (n - len) z
    | otherwise = xs
        where len = length xs

untabs :: [String] -> String
untabs =
    intercalate ['\t']

showInt :: Int -> String
showInt 0 = "-"
showInt x = show x

concatSpaces :: [String] -> String
concatSpaces =
    intercalate " "

-- INPUT

maybeRead :: Read a => String -> Maybe a
maybeRead str =
    case reads str of
      [(x, "")] -> Just x
      _         -> Nothing

prompt :: String -> (a -> String) -> [a] -> IO (Maybe a)
prompt a f xs = do
    putStrLn $ a ++ " is ambiguous."
    putStrLn $ "Perhaps you meant one of the following (q to quit):"

    let ys = zip [ (1 :: Int) .. ] xs
    forM_ ys $ \(i, x) -> do
        putStrLn $ show i ++ ") " ++ f x

    c <- getLine
    case c of
      "q" -> return Nothing
      mn  -> do
          case maybeRead mn of
            Nothing -> return Nothing
            Just n -> return $ Just $ xs !! (n - 1)

-- TIME

parseTime :: String -> Maybe UTCTime
parseTime str =
    case str of
      "0000-00-00" -> Nothing
      _            -> Just $ parseTimeOrError True defaultTimeLocale "%F" str

showTime :: UTCTime -> String
showTime =
    formatTime defaultTimeLocale "%F"

