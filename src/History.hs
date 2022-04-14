module History where

import Data.Aeson

data History = History { loan :: Int }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON History where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON History
