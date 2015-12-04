{-# LANGUAGE OverloadedStrings #-}

module MAL.API.Search
    ( searchFor
    ) where

import Control.Lens ( (&), (.~), (^.) )
import Data.String
import qualified Data.Text as T
import Network.HTTP.Types ( ok200 )
import Network.Wreq
import Text.XML
import Text.XML.Cursor

import MAL.Credentials
import MAL.Types
import Utils

searchFor :: Credentials -> String -> String -> IO [(Id, T.Text)]
searchFor creds ty req = do
    let url = concat [ "http://myanimelist.net/api/" ++ ty ++ "/search.xml" ]
        opts' = opts creds & param "q" .~ [T.pack req]
    r <- getWith opts' url
    if r ^. responseStatus /= ok200 then
        error "Error during the search"
    else do
        let cur      = fromDocument . parseLBS_ def $ r ^. responseBody
            rootAxis = element (fromString ty) &/ element "entry"
            ids      = cur $| rootAxis &/ element "id" &/ content
            entries  = cur $| rootAxis &/ element "title" &/ content
        return $ zip (map textToInt ids) entries

