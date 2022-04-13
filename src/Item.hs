module Item where

import Data.Aeson
import qualified Data.Csv as Csv

data Item = Item { name :: String, code :: String, serie :: String, price :: String, vendor :: String}
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Item

instance Csv.DefaultOrdered Item
instance Csv.FromNamedRecord Item
instance Csv.ToNamedRecord Item
