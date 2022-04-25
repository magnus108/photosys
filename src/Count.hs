module Count where

import           Data.Aeson


data Count = Count { item :: Int }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Count where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Count
