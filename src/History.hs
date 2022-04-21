module History where

import Data.Aeson
import Loan

data History = History { loan :: Loan , timestamp :: String   }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON History where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON History
