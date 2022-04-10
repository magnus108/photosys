{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe
import           Database


import qualified UserGui
import qualified LoanGui
import qualified ItemGui
import qualified LoginGui

import           Tab                            ( Tab(..) )
import qualified Tab

import qualified Data.ByteString               as BS


someFunc :: Int -> IO ()
someFunc port = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just "static"
                           , jsCustomHTML               = Just "index.html"
                           }
        $ setup



setup :: Window -> UI ()
setup window = void $ mdo
    --LoginGui.setup window
    --ItemGui.setup window
    --UserGui.setup window
    --LoanGui.setup window


    let datastore = "data/tab.json"
    database <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI
            (Database Tab)

    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem

    tab     <- dataItem bSelectionDataItem

    getBody window #+ [grid [[element listBox], [element tab]]]


    let eSelection = rumors $ UI.userSelection listBox

    bDatabase  <- accumB database $ concatenate <$> unions []
    bSelection <- stepper (Just 0) $ Unsafe.head <$> unions [eSelection]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = keys <$> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

    --let items = create (Tab ("Hand in")) $ create (Tab "Loan") emptydb
    --liftIO $ BS.writeFile datastore $ encode items

    return ()


type DataItem = Tab

showDataItem t = Tab.name t

emptyDataItem = Tab ""

dataItem :: Behavior (Maybe DataItem) -> UI Element
dataItem bItem = do
    entry <- UI.entry $ Tab.name . fromMaybe emptyDataItem <$> bItem

    element entry
