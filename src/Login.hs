module Login where

import           Data.Aeson

data Login = Login { name :: String, code :: String }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Login where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Login
