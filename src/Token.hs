module Token where

import           Data.Aeson

data Token = Token | NoToken
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Token where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Token


isToken :: Token -> Bool
isToken Token = True
isToken NoToken = False
