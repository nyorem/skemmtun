module Utils where

import Data.Function
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Read as T

organizeBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
organizeBy f =
    map (\xs -> (f . head $ xs, xs)) . groupBy ((==) `on` f)

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

