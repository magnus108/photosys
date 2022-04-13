{-# LANGUAGE RecursiveDo #-}
module Tab.Tab where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )
import qualified MenuBox

import           Token                          ( Token )
import qualified Token
import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import           Item                           ( Item )
import qualified Item
import           Tab                           ( Tab )
import qualified Tab

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
    -> Behavior (Database Tab)
    -> Behavior (Maybe DatabaseKey)
    -> UI Element -- (Element, Event (Database Item))
setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bSelectionToken bDatabaseTab bSelectionTab
    = mdo

    listBox <- MenuBox.listBox {-currentUser-} bListBoxItems {-bTabPairsFilter-} bSelection bDisplayDataItem

    elem <-
        UI.mkElement "nav"
        #. "navbar is-primary is-spaced"
        #+ [ UI.div
             #. "container"
             #+ [ element listBox
                , UI.div
                #. "navbar-menu"
                #+ [UI.div #. "navbar-start", UI.div #. "navbar-end" #+ [UI.div #. "navbar-item" #+ [{-element logoutBtn-}]]]
                ]
           ]

--    tab     <- dataItem bSelectionDataItem {-bSelectionDataItemFilter-} menu


    let eSelection = rumors $ MenuBox.userSelection listBox

    bSelection <- stepper (Just 5) $ Unsafe.head <$> unions [eSelection]

    let bLookup :: Behavior (DatabaseKey -> Maybe Tab)
        bLookup = flip Database.lookup <$> bDatabaseTab

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" Tab.name .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = Database.keys <$> bDatabaseTab


        bSelectionDataItem :: Behavior (Maybe Tab)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection


    {-
    let isAdmin = maybe False User.admin <$> bUser

        bTabPairs :: Behavior [(DatabaseKey, DataItem)]
        bTabPairs = toPairs <$> bDatabase

        bTabPairsFilter :: Behavior [DatabaseKey]
        bTabPairsFilter = fmap fst <$> ((\admin xs -> filter (\x -> admin == Tab.admin (snd x)) xs) <$> isAdmin <*> bTabPairs)

        bTabItemsFilter :: Behavior [Maybe DataItem]
        bTabItemsFilter = fmap <$> bLookup <*> bTabPairsFilter

        bSelectionDataItemFilter :: Behavior (Maybe DataItem)
        bSelectionDataItemFilter = (\x xs -> if elem x xs then x else Nothing) <$> bSelectionDataItem <*> bTabItemsFilter
        -}
    return elem
