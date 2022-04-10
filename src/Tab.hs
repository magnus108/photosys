module Tab where

import           Data.Aeson

data Tab = Tab { name :: String }
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Tab where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Tab
