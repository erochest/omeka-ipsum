{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import qualified Data.ByteString.Lazy as B
import           Data.Csv             hiding (Parser)
import qualified Data.Text            as T
import           Data.Time
import           Options.Applicative
import           Test.QuickCheck


data OmekaIpsum
        = OmekaIpsum
        { ipsumFile  :: !FilePath
        , outputFile :: !FilePath
        , randItems  :: !Int
        } deriving (Show)

data LoremIpsum
        = LoremIpsum
        { liParagraphs :: !Int
        , liSentences  :: !Int
        , liWords      :: !Int
        , loremIpsum   :: T.Text
        }

data OmekaRow
        = OmekaRow
        { rowTitle       :: !T.Text
        , rowSubject     :: !T.Text
        , rowDescription :: !T.Text
        , rowDate        :: !Day
        }

instance Arbitrary OmekaRow where
    arbitrary = undefined
    shrink _ = undefined

instance ToNamedRecord OmekaRow where
    toNamedRecord OmekaRow{..} = undefined


randomRows :: Int -> Gen [OmekaRow]
randomRows = vector

columnNames :: Header
columnNames = undefined

main :: IO ()
main = do
    OmekaIpsum{..} <- execParser opt
    generate (randomRows randItems)
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
