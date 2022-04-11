{-# LANGUAGE RecursiveDo #-}
module ItemGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString               as BS

import           Database

setup
    :: Window
    -> Behavior (Database DataItem)
    -> UI
           ( Element
           , Event ()
           , Behavior (Maybe DatabaseKey)
           , Event DataItem
           )
setup window bDatabase = mdo

    -- GUI elements
    createBtn                         <- UI.button #+ [string "Create"]
    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry                       <- UI.entry bFilterString

    ((elemName, elemCode), tDataItem) <- dataItem bSelectionDataItem


    -- GUI layout
    search                            <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterEntry #. "input" # set (attr "placeholder")
                                                     "Fx Kamera"
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
        #+ [ UI.label #. "label" #+ [string "Code"]
           , UI.div #. "control" #+ [element elemCode #. "input"]
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
        eDataItemIn = rumors $ tDataItem
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
        bShowDataItem = (maybe "" showDataItem .) <$> bLookup

        bShowDataItem2 :: Behavior (DatabaseKey -> String)
        bShowDataItem2   = (maybe "" name .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem2

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
    element elemCode # sink UI.enabled bDisplayItem

    return (elem, eCreate, bSelection, eDataItemIn)


{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = Item


showDataItem :: DataItem -> String
showDataItem item = name item ++ ", " ++ (code item)


emptyDataItem :: DataItem
emptyDataItem = Item "" ""


dataItem
    :: Behavior (Maybe DataItem) -> UI ((Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ name . fromMaybe emptyDataItem <$> bItem
    entry2 <- UI.entry $ code . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , Item <$> UI.userText entry1 <*> UI.userText entry2
        )
