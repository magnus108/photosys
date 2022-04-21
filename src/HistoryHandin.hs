module HistoryHandin where

import Data.Aeson
import Loan

data HistoryHandin = HistoryHandin { loan :: Loan , timestamp :: String }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON HistoryHandin where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON HistoryHandin
