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
    --UserGui.setup window
    --LoanGui.setup window


    let datastore = "data/tab.json"
    database <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI
            (Database Tab)

    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem

    menu    <- element listBox
    tab     <- dataItem bSelectionDataItem menu

    getBody window #+ [element tab]


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

dataItem :: Behavior (Maybe DataItem) -> Element -> UI Element
dataItem bItem tabs = do
    window  <- askWindow

    itemGui <- ItemGui.setup window
    loanGui <- LoanGui.setup window
    (loginGui, (loginBtn, logoutBtn), bLogin) <- LoginGui.setup window
    empty   <- string "fejl"


    login   <-
        UI.div
        #. "container"
        #+ [ grid
                 [ [element loginGui]
                 , [row [element loginBtn, element logoutBtn]]
                 ]
           ]

    let display y x = if y
            then case Tab.name x of
                "Loan"    -> [tabs, logoutBtn, itemGui]
                "Hand in" -> [tabs, logoutBtn, loanGui]
            else [login]

    let bGui = display <$> bLogin

    content <- UI.div # sink children (maybe [empty] <$> bGui <*> bItem)

    element content
