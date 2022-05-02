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

    , bSelectionToken :: !(Behavior (Maybe DatabaseKey))
    , bSelectionTab :: !(Behavior (Maybe DatabaseKey))

    , bCreateSelectionItem :: !(Behavior (Maybe DatabaseKey))
    }
