module Time where

import Data.Aeson

data Time = Time { time :: String }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Time where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Time
