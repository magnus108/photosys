{-# LANGUAGE RecursiveDo #-}
module Timer where

import           Data.Time

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )
import           Time
import           Monad
import           Env                            ( Env )

setup :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m) => Window -> m (Event Time)
setup window = mdo
    (eTime, hTime) <- liftIO $ newEvent

    timer <- liftUI $ UI.timer # set UI.interval 1000

    let eTick = UI.tick timer
    liftUI $ onEvent eTick $ \items -> do
        c <- liftIO $ (formatTime defaultTimeLocale "%F, %T") <$> getZonedTime
        liftIO $ hTime (Time c)

    liftUI $ UI.start timer

    return eTime


readTime :: String -> IO ZonedTime
readTime t = parseTimeM True defaultTimeLocale "%F, %T" t
