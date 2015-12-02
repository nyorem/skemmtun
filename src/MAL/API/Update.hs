{-# LANGUAGE OverloadedStrings #-}

module MAL.API.Update where

import Control.Lens ( (^.) )
import Network.HTTP.Types ( mkStatus )
import Network.Wreq

import MAL.Credentials
import MAL.Types

updateOK :: Status
updateOK =
    mkStatus 200 "Updated"

update :: (ToXML a) => Credentials -> a -> Id -> String -> IO ()
update creds x n ty = do
    let url = "http://myanimelist.net/api/" ++ ty ++ "list/update/" ++ show n ++ ".xml"
        opts' = opts creds
    r <- postWith opts' url [ "data" := toXml x ]
    let st = r ^. responseStatus
    if st /= updateOK then
        error "Error during the update"
    else
        return ()

