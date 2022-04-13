{-# LANGUAGE RecursiveDo #-}
module Export.Export where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Token                          ( Token )
import qualified Token
import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import           Item                           ( Item )
import qualified Item

import qualified Relude.Unsafe                 as Unsafe

import           Database

import qualified Data.List                     as List
import           Control.Bool


setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> Behavior (Database Token)
    -> Behavior (Maybe DatabaseKey)
    -> UI (Element, Event (Database Item))
setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bSelectionToken
    = mdo
        exportBtn  <- UI.button #+ [string "Export"]

        -- GUI layout
        exportBtn' <-
            UI.div
            #. "field"
            #+ [UI.div #. "control" #+ [element exportBtn #. "button"]]

        elem <-
            UI.div
            #. "section is-medium"
            #+ [UI.div #. "container" #+ [element exportBtn']]

        -- Events and behaviors
        let eExport = UI.click exportBtn

        return (elem, bDatabaseItem <@ eExport)
