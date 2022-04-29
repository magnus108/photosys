module Changes where

import           Data
import           Env
import           Monad
import           Graphics.UI.Threepenny.Core

changesCount
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesCount path = do
    bDatabase <- asks Env.bDatabaseCount
    liftUI $ onChanges bDatabase $ writeJson path


changesHistory
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesHistory path = do
    bDatabase <- asks Env.bDatabaseHistory
    liftUI $ onChanges bDatabase $ writeJson path

changesHistoryHandin
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesHistoryHandin path = do
    bDatabase <- asks Env.bDatabaseHistoryHandin
    liftUI $ onChanges bDatabase $ writeJson path

changesUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesUser path = do
    bDatabase <- asks Env.bDatabaseUser
    liftUI $ onChanges bDatabase $ writeJson path

changesTime
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesTime path = do
    bDatabase <- asks Env.bDatabaseTime
    liftUI $ onChanges bDatabase $ writeJson path

changesLoan
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesLoan path = do
    bDatabase <- asks Env.bDatabaseLoan
    liftUI $ onChanges bDatabase $ writeJson path

changesItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesItem path = do
    bDatabase <- asks Env.bDatabaseItem
    liftUI $ onChanges bDatabase $ writeJson path

changesToken
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => FilePath
    -> m ()
changesToken path = do
    bDatabase <- asks Env.bDatabaseToken
    liftUI $ onChanges bDatabase $ writeJson path

