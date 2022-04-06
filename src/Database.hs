module Database where


import qualified Relude.Container.Reexport     as Reexport
import qualified Relude.Extra.Map              as Map

type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Reexport.Map DatabaseKey a }
        deriving (Show)


emptydb = Database 0 mempty
keys = Map.keys . db

create x (Database newkey db) = Database (newkey + 1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey $ Map.insert key x db
delete key (Database newkey db) = Database newkey $ Map.delete key db
lookup key (Database _ db) = Map.lookup key db

