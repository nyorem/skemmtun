{-# LANGUAGE OverloadedStrings #-}

module MAL.API.Delete
    ( deleteAnime
    , deleteManga
    ) where

import Control.Monad ( void )
import Network.Wreq hiding ( delete )

import MAL.Credentials
import MAL.Types

delete :: String -> Credentials -> Id -> IO ()
delete ty creds n = do
    let url   = "http://myanimelist.net/api/" ++ ty ++ "list/delete/" ++ show n ++ ".xml"
        opts' = opts creds
    void $ getWith opts' url

deleteAnime :: Credentials -> Id -> IO ()
deleteAnime =
    delete "anime"

deleteManga :: Credentials -> Id -> IO ()
deleteManga =
    delete "manga"

