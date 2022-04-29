module Behaviors where

import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny
import           Database
import           Monad
import           User
import           Loan
import           Item                           ( Item )
import qualified Item
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

showUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> String))
showUser = do
    bLookup <- lookupUser
    return $ (maybe "" User.name .) <$> bLookup

showItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> String))
showItem = do
    bLookup <- lookupItem
    return $ (maybe "" Item.showItem .) <$> bLookup

showItemCode
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (DatabaseKey -> String))
showItemCode = do
    bLookup <- lookupItem
    return $ (maybe "" Item.code .) <$> bLookup

selectedToken
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (Maybe Token))
selectedToken = do
    bSelection <- asks Env.bSelectionToken
    bLookup    <- lookupToken
    return $ (=<<) <$> bLookup <*> bSelection

selectedItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Behavior (Maybe DatabaseKey) -> m (Behavior (Maybe Item))
selectedItem bSelection = do
    bLookup    <- lookupItem
    return $ (=<<) <$> bLookup <*> bSelection
