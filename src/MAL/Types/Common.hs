module MAL.Types.Common where

data MyStatus = Current
              | Completed
              | OnHold
              | Dropped
              | Planned
              deriving (Eq, Ord, Show)

toMyStatus :: Int -> Maybe MyStatus
toMyStatus 1 = Just Current
toMyStatus 2 = Just Completed
toMyStatus 3 = Just OnHold
toMyStatus 4 = Just Dropped
toMyStatus 6 = Just Planned
toMyStatus _ = Nothing

