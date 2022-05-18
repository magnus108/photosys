module Env where


import           Graphics.UI.Threepenny.Core
import           Token                          ( Token )
import qualified Token
import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import           Item                           ( Item )
import qualified Item
import           Count                          ( Count )
import qualified Count
import           Tab                            ( Tab )
import qualified Tab
import           History                        ( History )
import qualified History
import           HistoryHandin                  ( HistoryHandin )
import qualified HistoryHandin
import           Repair                         ( Repair )
import qualified Repair
import           Time                           ( Time )
import qualified Time
import           Database                       ( Database
                                                , DatabaseKey
                                                )

data Env = Env
    { bDatabaseLoan :: !(Behavior (Database Loan))
    , bDatabaseUser :: !(Behavior (Database User))
    , bDatabaseItem :: !(Behavior (Database Item))
    , bDatabaseToken :: !(Behavior (Database Token))
    , bDatabaseHistory :: !(Behavior (Database History))
    , bDatabaseHistoryHandin :: !(Behavior (Database HistoryHandin))
    , bDatabaseTab :: !(Behavior (Database Tab))
    , bDatabaseCount :: !(Behavior (Database Count))
    , bDatabaseTime :: !(Behavior (Database Time))
    , bDatabaseRepair :: !(Behavior (Database Repair))

    , bSelectionToken :: !(Behavior (Maybe DatabaseKey))
    , bSelectionTab :: !(Behavior (Maybe DatabaseKey))
    , bSelectionTime :: !(Behavior (Maybe DatabaseKey))

    , bCreateLoanFilterUser :: !(Behavior String)
    , bCreateLoanSelectionUser :: !(Behavior (Maybe DatabaseKey))
    , bCreateLoanFilterItem :: !(Behavior String)
    , bCreateLoanSelectionItem :: !(Behavior (Maybe DatabaseKey))
    , bCreateLoanModalState :: !(Behavior Bool)

    , bHistoryHandinLoan :: !(Behavior (Maybe DatabaseKey))
    , bHistoryHandinUser :: !(Behavior (Maybe DatabaseKey))
    , bHistoryHandinFilterUser :: !(Behavior String)
    , bHistoryHandinItem :: !(Behavior (Maybe DatabaseKey))

    , bDeleteLoanFilterUser :: !(Behavior String)
    , bDeleteLoanSelectionUser :: !(Behavior (Maybe DatabaseKey))
    , bDeleteLoanFilterItem :: !(Behavior String)
    , bDeleteLoanSelectionItem :: !(Behavior (Maybe DatabaseKey))
    }
