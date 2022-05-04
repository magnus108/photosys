module Repair where

import Data.Aeson
import Loan
import Time

data Repair = Repair { loan :: Int }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Repair where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Repair
