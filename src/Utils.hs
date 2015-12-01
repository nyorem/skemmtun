module Utils where

import Data.Function
import Data.List

organize :: (Eq b) => [(a, b)] -> [(b, [a])]
organize =
    map (\xs -> (snd . head $ xs, map fst xs)) . groupBy ((==) `on` snd)

newline :: IO ()
newline =
    putStrLn ""

