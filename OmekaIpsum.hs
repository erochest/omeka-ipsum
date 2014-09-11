{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Data.Aeson.Encode.Pretty   hiding (Config)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Maybe

import           OmekaIpsum.Options
import           OmekaIpsum.Types


main :: IO ()
main = runTask =<< execParser options

runTask :: OIOptions -> IO ()

runTask Config{..}   =
    B8.putStrLn . encodePretty
                $ OmekaConfig (fromMaybe (OmekaAuth "ADMIN_USER" "PASSWORD") _configAuth)
                              "http://localhost/omeka"

runTask Generate{..} = undefined

