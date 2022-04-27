module HistoryHandin where

import Data.Aeson
import Loan
import Time

data HistoryHandin = HistoryHandin { loan :: Loan , timestamp :: Time, adminUser :: Int }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON HistoryHandin where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON HistoryHandin
