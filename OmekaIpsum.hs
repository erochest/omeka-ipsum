{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson.Encode.Pretty   hiding (Config)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Default
import           Data.Maybe
import           Data.Traversable
import           Network.Wreq.Session       hiding (options)

import           OmekaIpsum.Config
import           OmekaIpsum.Omeka
import           OmekaIpsum.Options
import           OmekaIpsum.Types


main :: IO ()
main = runTask =<< execParser options

runTask :: OIOptions -> IO ()

runTask Config{..}   =
    let auth = fromMaybe (OmekaAuth "ADMIN_USER" "PASSWORD") _configAuth
    in  B8.putStrLn . encodePretty $ def & omekaAuth .~ auth

runTask Generate{..} = do
    c <-  updateConfig _generateAuth . fromMaybe def . join
      <$> traverse loadConfig _generateConfig
    print =<< withSession (runOmeka login c)
