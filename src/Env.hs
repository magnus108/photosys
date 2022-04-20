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
import           Tab                            ( Tab )
import qualified Tab
import           History                        ( History )
import qualified History
import           Database                       ( Database
                                                , DatabaseKey
                                                )

data Env = Env
    { logoutBtn :: !Element
    , export :: !Element
    , loanCreate :: !Element
    , loanDelete :: !Element
    , loanCreateNormal :: !Element
    , loanDeleteNormal :: !Element
    , history :: !Element
    , search :: !Element
    , tabs :: !Element
    , searchNormal :: !Element
    , userCreate :: !Element
    , userDelete :: !Element
    , itemCreate :: !Element
    , itemDelete :: !Element
    , tokenCreate :: !Element
    , bDatabaseLoan :: !(Behavior (Database Loan))
    , bDatabaseUser :: !(Behavior (Database User))
    , bDatabaseItem :: !(Behavior (Database Item))
    , bDatabaseToken :: !(Behavior (Database Token))
    , bSelectionToken :: !(Behavior (Maybe DatabaseKey))
    , bDatabaseHistory :: !(Behavior (Database History))
    , bDatabaseTab :: !(Behavior (Database Tab))
    , bSelectionTab :: !(Behavior (Maybe DatabaseKey))
    }
