module Database where

import Data.Aeson

import qualified Relude.Container.Reexport     as Reexport
import qualified Relude.Extra.Map              as Map

type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Reexport.Map DatabaseKey a }
        deriving (Show)
        deriving stock (Generic)

instance ToJSON a => ToJSON (Database a) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Database a)


emptydb = Database 0 mempty
keys = Map.keys . db

create x (Database newkey db) = Database (newkey + 1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey $ Map.insert key x db
delete key (Database newkey db) = Database newkey $ Map.delete key db
lookup key (Database _ db) = Map.lookup key db

