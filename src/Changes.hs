module Changes where

import qualified Timer
import qualified Time
import qualified Token
import           Utils.Data
import           Env
import           Behaviors
import           Monad
import           Graphics.UI.Threepenny.Core

import           Data.Time
import           Token

changesCount
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesCount path anyH = do
    bSelectedToken <- selectedToken
    bSelectedTime <- selectedTime
    bDatabase <- asks Env.bDatabaseCount
    liftUI $ onChanges bDatabase $ \x -> do
        mt <- updateToken bSelectedToken bSelectedTime
        case mt of
            Nothing -> return ()
            Just t -> liftIO $ anyH t
        writeJson path x


changesHistory
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesHistory path anyH = do
    bSelectedToken <- selectedToken
    bSelectedTime <- selectedTime
    bDatabase <- asks Env.bDatabaseHistory
    liftUI $ onChanges bDatabase $ \x -> do
        mt <- updateToken bSelectedToken bSelectedTime
        case mt of
            Nothing -> return ()
            Just t -> liftIO $ anyH t
        writeJson path x

changesHistoryHandin
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesHistoryHandin path anyH = do
    bSelectedToken <- selectedToken
    bSelectedTime <- selectedTime
    bDatabase <- asks Env.bDatabaseHistoryHandin
    liftUI $ onChanges bDatabase $ \x -> do
        mt <- updateToken bSelectedToken bSelectedTime
        case mt of
            Nothing -> return ()
            Just t -> liftIO $ anyH t
        writeJson path x

changesUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesUser path anyH = do
    bSelectedToken <- selectedToken
    bSelectedTime <- selectedTime
    bDatabase <- asks Env.bDatabaseUser
    liftUI $ onChanges bDatabase $ \x -> do
        mt <- updateToken bSelectedToken bSelectedTime
        case mt of
            Nothing -> return ()
            Just t -> liftIO $ anyH t
        writeJson path x

changesTime
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesTime path anyH = do
    bDatabase      <- asks Env.bDatabaseTime
    liftUI $ onChanges bDatabase $ writeJson path

changesLoan
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesLoan path anyH = do
    bSelectedToken <- selectedToken
    bSelectedTime <- selectedTime
    bDatabase <- asks Env.bDatabaseLoan
    liftUI $ onChanges bDatabase $ \x -> do
        mt <- updateToken bSelectedToken bSelectedTime
        case mt of
            Nothing -> return ()
            Just t -> liftIO $ anyH t
        writeJson path x

changesItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesItem path anyH = do
    bSelectedToken <- selectedToken
    bSelectedTime <- selectedTime
    bDatabase <- asks Env.bDatabaseItem
    liftUI $ onChanges bDatabase $ \x -> do
        mt <- updateToken bSelectedToken bSelectedTime
        case mt of
            Nothing -> return ()
            Just t -> liftIO $ anyH t
        writeJson path x

changesToken
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> Handler Token
    -> m ()
changesToken path anyH = do
    bDatabase <- asks Env.bDatabaseToken
    liftUI $ onChanges bDatabase $ writeJson path


updateToken :: (MonadIO m) => Behavior (Maybe Token) -> Behavior (Maybe Time.Time) -> m (Maybe Token)
updateToken bSelectedToken bSelectedTime = do
    token <- liftIO $ currentValue bSelectedToken
    time <- liftIO $ currentValue bSelectedTime
    return $ case token of
        Nothing -> Nothing
        Just Token.NoToken -> Nothing
        Just (Token.Token a _) -> Just $ Token.Token a (fromMaybe (Time.Time "") time)
