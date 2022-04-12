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
elems = Map.elems . db
toPairs = Map.toPairs . db

create x (Database newkey db) = Database (newkey + 1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey $ Map.insert key x db
delete key (Database newkey db) = Database newkey $ Map.delete key db
lookup key (Database _ db) = Map.lookup key db

findIndex f (Database _ db) = fst <$> find (f . snd) (Map.toPairs db)


update' mkey x = flip update x <$> mkey

updateMaybe key Nothing db = db
updateMaybe key (Just x) db = update key x db
