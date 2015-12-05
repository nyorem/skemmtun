{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MAL.API.Add
    ( addAnime
    , addManga
    ) where

import Control.Monad ( void )
import Data.Default
import Data.Proxy
import Network.Wreq hiding ( Proxy )

import MAL.Credentials
import MAL.Types

add :: forall proxy a. (Default a, ToXML a) => proxy a -> String -> Credentials -> Id -> IO ()
add _ ty creds n = do
    let url   = concat [ "http://myanimelist.net/api/" ++ ty ++ "list/add/" ++ show n ++ ".xml" ]
        opts' = opts creds
        o     = def :: a
    void $ postWith opts' url [ "data" := toXml o ]

addAnime :: Credentials -> Id -> IO ()
addAnime =
    add (Proxy :: Proxy Anime) "anime"

addManga :: Credentials -> Id -> IO ()
addManga =
    add (Proxy :: Proxy Manga) "manga"

