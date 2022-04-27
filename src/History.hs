module History where

import Data.Aeson
import Loan
import Time

data History = History { loan :: Loan , timestamp :: Time, adminUser :: Int}
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON History where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON History
