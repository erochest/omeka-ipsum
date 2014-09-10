{-# LANGUAGE OverloadedStrings #-}


module OmekaIpsum.Options
    ( options
    , execParser
    ) where


import qualified Data.ByteString.UTF8 as B8
import qualified Data.Text            as T
import           Data.Version
import           Options.Applicative

import           OmekaIpsum.Types
import           Paths_omeka_ipsum


versionStr :: String
versionStr = showVersion version

authOptions' :: Parser (Maybe OmekaAuth)
authOptions' =   liftA2 OmekaAuth
             <$> optional (option (return . B8.fromString)
                                  (  short 'u' <> long "user"
                                  <> help "The Omeka admin user name."))
             <*> optional (option (return . B8.fromString)
                                  (  short 'p' <> long "password"
                                  <> help "The Omeka admin password."))

configOptions' :: Parser OIOptions
configOptions' =   Config
               <$> authOptions'

configOptions :: ParserInfo OIOptions
configOptions =
    info (helper <*> configOptions')
         (  fullDesc
         <> progDesc "Dump a sample, default configuration file.")

generateOptions' :: Parser OIOptions
generateOptions' =   Generate
                 <$> authOptions'
                 <*> option auto (  short 'c' <> long "config" <> metavar "CONFIG_FILE"
                                 <> help "A configuration file.")

generateOptions :: ParserInfo OIOptions
generateOptions =
    info (helper <*> generateOptions')
         (  fullDesc
         <> progDesc "Generate random data and insert it into Omeka.")

options' :: Parser OIOptions
options' = subparser (  command "config"   configOptions
                     <> command "generate" generateOptions
                     )

options :: ParserInfo OIOptions
options = info (helper <*> options')
               (  fullDesc
               <> progDesc "Generates random content for testing Omeka themes and plugins."
               <> header ("omeka-ipsum " ++ versionStr ++ " Random item generator.")
               )
