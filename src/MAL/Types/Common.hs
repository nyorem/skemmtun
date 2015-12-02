module MAL.Types.Common where

import qualified Data.ByteString as BS

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

fromMyStatus :: Maybe MyStatus -> Int
fromMyStatus (Just Current)   = 1
fromMyStatus (Just Completed) = 2
fromMyStatus (Just OnHold)    = 3
fromMyStatus (Just Dropped)   = 4
fromMyStatus (Just Planned)   = 6
fromMyStatus Nothing          = 0

type Id = Int

-- | Serialize a type to XML.
class ToXML a where
    toXml :: a -> BS.ByteString

