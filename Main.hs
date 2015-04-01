{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import qualified Data.ByteString.Lazy as B
import           Data.Csv             hiding (Parser)
import qualified Data.Text            as T
import           Data.Time
import           Options.Applicative
import           System.Random.MWC


data OmekaIpsum
        = OmekaIpsum
        { ipsumFile  :: !FilePath
        , outputFile :: !FilePath
        , randItems  :: !Int
        } deriving (Show)

data LoremIpsum
        = LoremIpsum

data OmekaRow
        = OmekaRow
        { rowTitle       :: !T.Text
        , rowSubject     :: !T.Text
        , rowDescription :: !T.Text
        , rowDate        :: !OmekaDay
        }

newtype OmekaDay = OmekaDay { getDay :: Day }

instance ToField OmekaDay where
    toField = undefined

instance ToNamedRecord OmekaRow where
    toNamedRecord OmekaRow{..} =
        namedRecord [ "Title"       .= rowTitle
                    , "Subject"     .= rowSubject
                    , "Description" .= rowDescription
                    , "Date"        .= rowDate
                    ]


columnNames :: Header
columnNames = ["Title", "Subject", "Description", "Date"]

loremIpsum :: LoremIpsum -> Int -> Int -> Int -> GenIO -> IO T.Text
loremIpsum = undefined

randomDay :: GenIO -> IO Day
randomDay = undefined


randomOmekaRow :: GenIO -> IO OmekaRow
randomOmekaRow = undefined

randomRows :: Int -> GenIO -> IO [OmekaRow]
randomRows = undefined

main :: IO ()
main = do
    OmekaIpsum{..} <- execParser opt
    (withSystemRandom . asGenIO $ randomRows randItems)
        >>= B.writeFile outputFile . encodeByName columnNames


opt' :: Parser OmekaIpsum
opt' =   OmekaIpsum
     <$> strOption   (  short 'o' <> long "output" <> metavar "FILENAME"
                     <> help "The file name to write the output to.")
     <*> strOption   (  short 'f' <> long "ipsum-file" <> metavar "FILENAME"
                     <> help "The file to read to generate lorem ipsum from.")
     <*> option auto (  short 'i' <> long "items" <> metavar "ITEM_COUNT"
                     <> value 100
                     <> help "The number of random items to generate.\
                             \ Default = 100.")

opt :: ParserInfo OmekaIpsum
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Generate random items for importing into Omeka."
           <> header "omeka-ipsum -- random Omeka data.")
