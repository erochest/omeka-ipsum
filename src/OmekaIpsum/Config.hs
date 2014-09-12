{-# LANGUAGE OverloadedStrings #-}


module OmekaIpsum.Config
    ( loadConfig
    , updateConfig
    ) where


import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString  as B

import           OmekaIpsum.Types


loadConfig :: FilePath -> IO (Maybe OmekaConfig)
loadConfig = fmap decodeStrict . B.readFile

updateConfig :: Maybe OmekaAuth -> OmekaConfig -> OmekaConfig
updateConfig Nothing  = id
updateConfig (Just a) = set omekaAuth a
