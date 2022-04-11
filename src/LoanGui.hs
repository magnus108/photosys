{-# LANGUAGE RecursiveDo #-}
module LoanGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Loan
import           User (User)
import qualified User
import           Item (Item)
import qualified Item

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString               as BS

import           Database


setup
    :: Window
    -> Behavior (Database DataItem)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> UI
           ( Element
           , Event ()
           , Behavior (Maybe DatabaseKey)
           , Event DataItem
           )
setup window bDatabase bDatabaseUser bDatabaseItem = mdo

    -- GUI elements
    createBtn                         <- UI.button #+ [string "Create"]
    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry                       <- UI.entry bFilterString

    ((elemName, elemItem), tDataItem) <- dataItem bSelectionDataItem

    -- GUI layout
    search                            <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterEntry #. "input" # set
                    (attr "placeholder")
                    "Fx Anders Andersen eller Kamera"
              ]
           ]

    dropdown <-
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBox # set (attr "size") "8" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    button <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]


    uiDataName <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Name"]
           , UI.div
           #. "control"
           #+ [ element elemName #. "input" # set (attr "placeholder")
                                                  "Fx Kamera 1"
              ]
           ]

    uiDataPassword <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Item"]
           , UI.div #. "control" #+ [element elemItem #. "input"]
           ]


    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element search
                , element dropdown
                , element uiDataName
                , element uiDataPassword
                , element button
                ]
           ]



    -- Events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry

    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilter = isInfixOf <$> UI.userText filterEntry
        bFilter = facts tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDataItemIn = filterJust $ rumors $ tDataItem
        eCreate     = UI.click createBtn



    bSelection <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelection
        , Just . nextKey <$> bDatabase <@ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelection
        <*> bShowDataItem
        <@> eFilter
        ]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilter
                <*> bShowDataItem
                <*> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection


    let bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bSelection

    element elemName # sink UI.enabled bDisplayItem
    element elemItem # sink UI.enabled bDisplayItem


    return (elem, eCreate, bSelection, eDataItemIn)

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = Loan

showDataItem :: DataItem -> String
showDataItem i = show (item i) ++ ", " ++ (show (user i))


dataItem
    :: Behavior (Maybe DataItem)
    -> UI ((Element, Element), Tidings (Maybe DataItem))
dataItem bItem = do
    entry1 <- UI.entry $ maybe "" (show . item) <$> bItem
    entry2 <- UI.entry $ maybe "" (show . user) <$> bItem

    let maybeParse1 = readMaybe <$> UI.userText entry2
    let maybeParse2 = readMaybe <$> UI.userText entry2
    return
        ( (getElement entry1, getElement entry2)
        , liftA2 Loan <$> maybeParse1 <*> maybeParse2
        )
