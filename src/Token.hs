module Token where

import           Data.Aeson

data Token = Token Int String | NoToken
    deriving stock (Eq, Ord, Show)
    deriving stock (Generic)

instance ToJSON Token where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Token


isToken :: Token -> Bool
isToken (Token _ _) = True
isToken NoToken = False

tokenId :: Token -> Maybe Int
tokenId (Token x _) = Just x
tokenId NoToken = Nothing

tokenTTL :: Token -> Maybe String
tokenTTL (Token _ y) = Just y
tokenTTL NoToken = Nothing
