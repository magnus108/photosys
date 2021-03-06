module Loan where

import Data.Aeson

data Loan = Loan { item :: Int, user :: Int}
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Loan where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Loan
