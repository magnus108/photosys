module Token where

import           Data.Aeson

data Token = Token Int | NoToken
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Token where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Token


isToken :: Token -> Bool
isToken (Token _) = True
isToken NoToken = False

tokenId :: Token -> Maybe Int
tokenId (Token x) = Just x
tokenId NoToken = Nothing
