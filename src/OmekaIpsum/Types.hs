{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


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

    , OmekaConfig(..)
    , omekaAuth
    , omekaURL
    ) where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.UTF8 as B8
import           Data.Char
import qualified Data.Text            as T


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

data OmekaConfig = OmekaConfig
                 { _omekaAuth :: OmekaAuth
                 , _omekaURL  :: T.Text
                 } deriving (Show)
$(makeLenses ''OmekaConfig)

instance FromJSON ByteString where
    parseJSON (String s) = return . B8.fromString $ T.unpack s
    parseJSON _          = mzero

instance ToJSON ByteString where
    toJSON = String . T.pack . B8.toString

$(deriveJSON
    defaultOptions { fieldLabelModifier     = drop 6
                   , constructorTagModifier = map toLower
                   }
    ''OmekaAuth)
$(deriveJSON
    defaultOptions { fieldLabelModifier     = drop 6
                   , constructorTagModifier = map toLower
                   }
    ''OmekaConfig)
