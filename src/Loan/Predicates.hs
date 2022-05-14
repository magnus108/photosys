module Loan.Predicates where

import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import Data.Functor.Contravariant.Divisible
import           Env                            ( Env )
import qualified Env

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny
import           Database

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



data DeleteLoanFilter = DeleteLoanFilter
    { lookupLoan :: (DatabaseKey -> Maybe Loan)
    , selectionUser :: Maybe DatabaseKey
    , selectionItem :: Maybe DatabaseKey
    , filterUser :: (String -> Bool)
    , showUser :: (DatabaseKey -> String)
    , showItem :: (DatabaseKey -> String)
    , filterItem :: (String -> Bool)
    }

compareMaybe :: Eq a => Maybe a -> Predicate a
compareMaybe = Predicate . flip (maybe True . (==))

false :: Predicate a
false = Predicate (const False)

chooseMaybe :: Predicate c -> Predicate (Maybe c)
chooseMaybe = choose (maybeToRight ()) false

pDivideLoan :: Divisible f0 => f0 Int -> f0 Int -> f0 Loan
pDivideLoan = divide (\l -> (Loan.item l, Loan.user l))

    {-
pShowUser
    :: forall m f0
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m, Contravariant f0)
      => m (Behavior (f0 String -> f0 DatabaseKey))
pShowUser = do
    bShow <- showUser
    return $ contramap <$> bShow

pShowItem
    :: forall m f0
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m, Contravariant f0)
      => m (Behavior (f0 String -> f0 DatabaseKey))
pShowItem = do
    bShow <- showItem
    return $ contramap <$> bShow

pLookupLoan
    :: forall m f0
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m, Contravariant f0)
      => m (Behavior (f0 (Maybe Loan) -> f0 DatabaseKey))
pLookupLoan = do
    bLookupLoan     <- lookupLoan
    return $ contramap <$> bLookupLoan


pCompareDeleteLoanSelectionItem
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m )
      => m (Behavior (Predicate DatabaseKey))
pCompareDeleteLoanSelectionItem = do
    bSelection <- asks Env.bDeleteLoanSelectionItem
    return $ compareMaybe <$> bSelection


pCompareDeleteLoanSelectionUser
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m )
      => m (Behavior (Predicate DatabaseKey))
pCompareDeleteLoanSelectionUser = do
    bSelection <- asks Env.bDeleteLoanSelectionUser
    return $ compareMaybe <$> bSelection
    -}
