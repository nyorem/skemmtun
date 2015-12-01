module MAL.API.Verify where

import Control.Exception
import Control.Monad ( void )
import Network.HTTP.Client ( HttpException(..) )
import Network.Wreq

import MAL.Credentials

-- Check if the credentials are correct
verifyCredentials :: Credentials -> IO ()
verifyCredentials creds = do
    void $ getWith (opts creds) "http://myanimelist.net/api/account/verify_credentials.xml" `catch` handler
    where handler (StatusCodeException _ _ _) = error "Credentials verification failed!"
          handler _                           = error "Credentials verification failed!"

