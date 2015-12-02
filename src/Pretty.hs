module Pretty where

-- see: http://stackoverflow.com/questions/5929377/format-list-output-in-haskell
import Data.List ( transpose, intercalate )

-- a type for fill functions
type Filler = Int -> String -> String

-- a type for describing table columns
data ColDesc t = ColDesc { colTitleFill :: Filler
                         , colTitle     :: String
                         , colValueFill :: Filler
                         , colValue     :: t -> String
                         }

-- functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft, fillRight, fillCenter :: a -> Int -> [a] -> [a]
fillLeft c n s   = s ++ replicate (n - length s) c
fillRight c n s  = replicate (n - length s) c ++ s
fillCenter c n s = replicate l c ++ s ++ replicate r c
    where x = n - length s
          l = x `div` 2
          r = x - l

-- functions that fill with spaces
left, right, center :: Int -> String -> String
left   = fillLeft ' '
right  = fillRight ' '
center = fillCenter ' '

-- converts a list of items into a table according to a list
-- of column descriptors
ppTable :: [ColDesc t] -> [t] -> String
ppTable cs ts =
    let header = map colTitle cs
        rows = [[colValue c t | c <- cs] | t <- ts]
        widths = [maximum $ map length col | col <- transpose $ header : rows]
        separator = intercalate "-+-" [replicate width '-' | width <- widths]
        fillCols fill cols = intercalate " | " [fill c width col | (c, width, col) <- zip3 cs widths cols]
    in
        unlines $ fillCols colTitleFill header : separator : map (fillCols colValueFill) rows

renderTable :: [ColDesc t] -> [t] -> IO ()
renderTable cs =
    putStrLn . ppTable cs

