module User where

import Data.Aeson

data User = User { name :: String, code :: String, admin :: Bool }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User
