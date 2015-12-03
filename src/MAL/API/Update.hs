{-# LANGUAGE OverloadedStrings #-}

module MAL.API.Update
    ( update
    ) where

import Control.Lens ( (^.) )
import qualified Data.Text as T
import Network.HTTP.Types ( mkStatus )
import Network.Wreq

import MAL.Credentials
import MAL.Types
import Utils

updateOK :: Status
updateOK =
    mkStatus 200 "Updated"

update' :: (ToXML a) => Credentials -> a -> Id -> String -> IO ()
update' creds x n ty = do
    let url = "http://myanimelist.net/api/" ++ ty ++ "list/update/" ++ show n ++ ".xml"
        opts' = opts creds
    r <- postWith opts' url [ "data" := toXml x ]
    let st = r ^. responseStatus
    if st /= updateOK then
        error "Error during the update"
    else
        return ()

-- Generic function for updating stuff
update :: (ToXML a)
       => Credentials
       -> T.Text
       -> (Credentials -> String -> IO [a])
       -> (a -> T.Text)
       -> (a -> Id)
       -> (a -> a)
       -> String
       -> IO ()
update creds name retrieve toText toId action prefix = do
    let uname = fst creds
    objects <- retrieve creds uname
    let mobject = findPrefixes 5 (T.unpack . toText) (T.unpack name) objects
    case mobject of
      [] -> error $ prefix ++ " " ++ (T.unpack name) ++ " not found!"
      xs -> do
          mx <- prompt (T.unpack name) (T.unpack . toText) xs
          case mx of
            Nothing -> putStrLn "Not updated!"
            Just o -> update' creds (action o) (toId o) (toLowers prefix) >> putStrLn "Updated!"

