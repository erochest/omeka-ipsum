{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module OmekaIpsum.Types
    ( OIOptions(..)
    , _Config
    , configAuth
    , _Generate
    , generateAuth
    , generateConfig

    , OmekaAuth(..)
    , omekaAuthUser
    , omekaAuthPass
    ) where


import           Control.Lens
import           Data.ByteString (ByteString)


data OmekaAuth = OmekaAuth
               { _omekaAuthUser :: ByteString
               , _omekaAuthPass :: ByteString
               } deriving (Show)
$(makeLenses ''OmekaAuth)

data OIOptions = Config   { _configAuth :: Maybe OmekaAuth }
               | Generate { _generateAuth   :: Maybe OmekaAuth
                          , _generateConfig :: FilePath
                          }
               deriving (Show)
$(makeLenses ''OIOptions)
$(makePrisms ''OIOptions)

