module Data where

import qualified Data.ByteString               as BS
import qualified Relude.Unsafe                 as Unsafe
import qualified Data.Csv                      as Csv
import           Data.Aeson

readJson :: (MonadIO m, FromJSON a) => FilePath -> m a
readJson fp = liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

writeJson :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJson fp items = liftIO $ BS.writeFile fp $ toStrict $ encode items

writeCsv
    :: (MonadIO m, Csv.DefaultOrdered a, Csv.ToNamedRecord a)
    => FilePath
    -> [a]
    -> m ()
writeCsv fp items =
    liftIO $ BS.writeFile fp $ toStrict $ Csv.encodeDefaultOrderedByName items
