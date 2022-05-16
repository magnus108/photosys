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
