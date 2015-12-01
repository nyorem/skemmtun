module MAL.Credentials where

import Control.Lens
import Data.String
import Network.Wreq

-- (username, password)
type Credentials =
    (String, String)

-- Config file should have the following format:
-- username = foo
-- password = bar
readCredentials :: FilePath -> IO Credentials
readCredentials file = do
    contents <- readFile file
    let ls = lines contents
        (u, p) = (head ls, head . tail $ ls)
        extract = head . tail . words . snd . span (/= '=')
    return $ (extract u, extract p)

-- Authentification credentials
opts :: Credentials -> Options
opts (username, password) =
    defaults & auth ?~ basicAuth (fromString username) (fromString password)

