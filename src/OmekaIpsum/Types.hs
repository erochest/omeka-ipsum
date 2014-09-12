{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module OmekaIpsum.Types
    ( OIOptions(..)
    , _Config
    , configAuth
    , _Generate
    , generateAuth
    , generateConfig
    , generateN

    , OmekaAuth(..)
    , omekaAuthUser
    , omekaAuthPass

    , OmekaConfig(..)
    , omekaAuth
    , omekaURL

    , Omeka
    , runOmeka
    , runOmeka'
    , config
    , session
    ) where


import           Control.Applicative
import           Control.Error
import qualified Control.Exception          as Ex
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.UTF8       as B8
import           Data.Char
import           Data.Default
import qualified Data.Text                  as T
import           Network.Wreq.Session


data OmekaAuth = OmekaAuth
               { _omekaAuthUser :: ByteString
               , _omekaAuthPass :: ByteString
               } deriving (Show)
$(makeLenses ''OmekaAuth)

instance Default OmekaAuth where
    def = OmekaAuth "" ""

data OIOptions = Config   { _configAuth :: Maybe OmekaAuth }
               | Generate { _generateAuth   :: Maybe OmekaAuth
                          , _generateConfig :: Maybe FilePath
                          , _generateN      :: Int
                          }
               deriving (Show)
$(makeLenses ''OIOptions)
$(makePrisms ''OIOptions)

data OmekaConfig = OmekaConfig
                 { _omekaAuth :: OmekaAuth
                 , _omekaURL  :: T.Text
                 } deriving (Show)
$(makeLenses ''OmekaConfig)

instance Default OmekaConfig where
    def = OmekaConfig def "http://localhost/"

data OmekaSession = OS
                  { _config  :: OmekaConfig
                  , _session :: Session
                  }
$(makeLenses ''OmekaSession)

newtype Omeka a = Omeka
                  { unOmeka :: EitherT Ex.SomeException (ReaderT OmekaSession IO) a
                  } deriving (Functor, Applicative, Monad)

instance MonadIO Omeka where
    liftIO = Omeka . liftIO

instance MonadReader OmekaSession Omeka where
    ask     = Omeka ask
    local f = Omeka . local f . unOmeka

instance MonadError Ex.SomeException Omeka where
    throwError = Omeka . EitherT . return . Left
    catchError a handler = join
                         . fmap (handler' handler)
                         . Omeka . lift . lift
                         . runReaderT (runEitherT $ unOmeka a)
                         =<< ask

handler' :: (Ex.SomeException -> Omeka a)
         -> Either Ex.SomeException a
         -> Omeka a
handler' _ (Right v) = return v
handler' f (Left e)  = f e

runSession' :: Omeka a -> OmekaSession -> IO (Either Ex.SomeException a)
runSession' a = runReaderT (runEitherT $ unOmeka a)

runOmeka :: Omeka a -> OmekaConfig -> Session
         -> IO (Either Ex.SomeException a)
runOmeka a c = runSession' a . OS c

runOmeka' :: OmekaConfig -> Omeka a -> Session
          -> IO (Either Ex.SomeException a)
runOmeka' c a s = runOmeka a c s

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
