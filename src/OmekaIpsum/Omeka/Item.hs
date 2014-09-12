{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module OmekaIpsum.Omeka.Item
    ( Item(..)
    ) where


import Control.Lens
import qualified Data.Text as T
import Data.Time
import Data.Default


data Item = Item
          { _itemTitle       :: T.Text
          , _itemSubject     :: T.Text
          , _itemDescription :: T.Text
          , _itemCreator     :: T.Text
          , _itemSource      :: T.Text
          , _itemPublisher   :: T.Text
          , _itemDate        :: UTCTime
          , _itemContributor :: T.Text
          , _itemRights      :: T.Text
          , _itemRelation    :: T.Text
          , _itemFormat      :: T.Text
          , _itemLanguage    :: T.Text
          , _itemType        :: T.Text
          , _itemIdentifier  :: T.Text
          , _itemCoverage    :: T.Text
          , _itemPublic      :: Bool
          , _itemFeatures    :: Bool
          , _itemCollection  :: Maybe Int
          }
