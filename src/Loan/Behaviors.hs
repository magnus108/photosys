module Loan.Behaviors where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny
import           Database
import           Monad
import           User
import           Utils.Utils
import           Loan
import           Item                           ( Item )
import qualified Item
import           Time                           ( Time )
import qualified Time
import           Count                          ( Count )
import qualified Count
import           HistoryHandin                  ( HistoryHandin )
import qualified HistoryHandin
import           Repair                         ( Repair )
import qualified Repair
import           Token
import           Env                            ( Env )
import qualified Env
import Behaviors

import           Control.Bool


selectedUserDelete
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (Maybe User))
selectedUserDelete = do
    bSelection <- asks Env.bDeleteLoanSelectionUser
    bLookup    <- lookupUser
    return $ (=<<) <$> bLookup <*> bSelection


selectedItemDelete
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (Maybe Item))
selectedItemDelete = do
    bSelection <- asks Env.bDeleteLoanSelectionItem
    bLookup    <- lookupItem
    return $ (=<<) <$> bLookup <*> bSelection

showItemDelete
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior String)
showItemDelete = do
    bSelection <- asks Env.bDeleteLoanSelectionItem
    bShow    <- showItem
    return $ maybe "" <$> bShow <*> bSelection

selectedCreateLoanItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (Maybe Item))
selectedCreateLoanItem = do
    bSelection <- asks Env.bCreateLoanSelectionItem
    bLookup    <- lookupItem
    return $ (=<<) <$> bLookup <*> bSelection

selectedCreateLoanUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (Maybe Item))
selectedCreateLoanUser = do
    bSelection <- asks Env.bCreateLoanSelectionUser
    bLookup    <- lookupItem
    return $ (=<<) <$> bLookup <*> bSelection

hasSelectedCreateLoanItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior Bool)
hasSelectedCreateLoanItem = do
    bSelection <- selectedCreateLoanItem
    return $ isJust <$> bSelection


hasSelectedCreateLoanUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior Bool)
hasSelectedCreateLoanUser = do
    bSelection <- selectedCreateLoanUser
    return $ isJust <$> bSelection

canCreateLoan
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior Bool)
canCreateLoan = do
    hasItemSelected <- hasSelectedCreateLoanItem
    hasUserSelected <- hasSelectedCreateLoanUser
    return $ hasUserSelected <&&> hasItemSelected

createLoan
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Behavior (Maybe Loan))
createLoan = do
    bSelectionItem  <- asks Env.bCreateLoanSelectionItem
    bSelectionUser    <- asks Env.bCreateLoanSelectionUser
    return $ liftA2 Loan.Loan <$> bSelectionItem <*> bSelectionUser
