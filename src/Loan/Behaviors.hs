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
