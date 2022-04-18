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
import           History                        ( History )
import qualified History
import           Database                       ( Database
                                                , DatabaseKey
                                                )

data Env = Env
    { bDatabaseLoan :: Behavior (Database Loan)
    , bDatabaseUser :: Behavior (Database User)
    , bDatabaseItem :: Behavior (Database Item)
    , bDatabaseToken :: Behavior (Database Token)
    , bSelectionToken :: Behavior (Maybe DatabaseKey)
    , bDatabaseHistory :: Behavior (Database History)
    }
