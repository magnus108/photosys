module Behaviors where

import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny
import           Database
import           Monad
import           User
import           Loan
import           Item
import           Token
import           Env


lookupUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> Maybe User))
lookupUser = do
    bDatabase <- asks Env.bDatabaseUser
    return $ flip lookup <$> bDatabase


lookupLoan
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> Maybe Loan))
lookupLoan = do
    bDatabase <- asks Env.bDatabaseLoan
    return $ flip lookup <$> bDatabase

lookupItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> Maybe Item))
lookupItem = do
    bDatabase <- asks Env.bDatabaseItem
    return $ flip lookup <$> bDatabase

lookupToken
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> Maybe Token))
lookupToken = do
    bDatabase <- asks Env.bDatabaseToken
    return $ flip lookup <$> bDatabase
