module Item where

import Data.Aeson

data Item = Item { name :: String, code :: String }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Item
