{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module OmekaIpsum.Omeka
    ( login
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Monoid
import qualified Data.Text                  as T
import           Network.Wreq.Lens
import           Network.Wreq.Session
import           Network.Wreq.Types

import           OmekaIpsum.Types

login :: Omeka ()
login = do
    loginUrl      <- T.unpack . (<> "/admin/users/login") <$> view (config . omekaURL)
    OmekaAuth{..} <- view (config . omekaAuth)
    s             <- view session
    r             <- liftIO $ post s loginUrl [ "username" := _omekaAuthUser
                                              , "password" := _omekaAuthPass
                                              , "remember" := ("1" :: B.ByteString)
                                              , "submit"   := ("Log In" :: B.ByteString)
                                              ]
    liftIO . B8.putStrLn $ r ^. responseBody
