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
import           Monad
import           Env                            ( Env )
import qualified Env


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event (Database Item))
setup window = mdo
    bDatabaseItem   <- asks Env.bDatabaseItem

    -- GUI elements
    exportBtn  <- liftUI $ UI.button #+ [string "Export"]

    -- GUI layout
    exportBtn' <- liftUI $
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element exportBtn #. "button"]]

    elem <- liftUI $
        UI.div
        #. "section is-medium"
        #+ [UI.div #. "container" #+ [element exportBtn']]

    -- Events and behaviors
    let eExport = UI.click exportBtn

    return (elem, bDatabaseItem <@ eExport)
