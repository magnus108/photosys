{-# LANGUAGE RecursiveDo #-}
module Item.Create where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

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
    -> UI (Element, Event Item)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem = mdo

    -- GUI elements
    filterItem                        <- UI.entry bFilterEntryItem
    listBoxItem <- UI.listBox bListBoxItems bSelectionItem bDisplayItemName

    ((elemName, elemCode), tDataItem) <- dataItem bSelectedItem
    createBtn                         <- UI.button #+ [string "Opret"]

    -- GUI layout
    searchItem                        <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterItem #. "input" # set (attr "placeholder")
                                                    "Fx Kamera"
              ]
           ]

    dropdownItem <-
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBoxItem # set (attr "size") "5" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    dataName <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Name"]
           , UI.div
           #. "control"
           #+ [ element elemName #. "input" # set (attr "placeholder")
                                                  "Fx Kamera 1"
              ]
           ]

    dataCode <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Code"]
           , UI.div #. "control" #+ [element elemCode #. "input"]
           ]


    createBtn' <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]


    closeBtn <- UI.button #. "modal-close is-large"
    modal    <-
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Opret godkendt"]]
               , element closeBtn
               ]

    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchItem
                , element dropdownItem
                , element dataName
                , element dataCode
                , element createBtn'
                , element modal
                ]
           ]


    -- Events and behaviors
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem


    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionItem = rumors $ UI.userSelection listBoxItem
        eItemIn        = rumors $ tDataItem
        eCreate        = UI.click createBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]


    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowItem
        <@> eFilterItem
        , Nothing <$ eCreate
        ]


    let bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

        bSelectedItem :: Behavior (Maybe Item)
        bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.name .) <$> bLookupItem

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilterItem
                <*> bShowItem
                <*> bDatabaseItem

    let bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bSelectionItem

    element elemName # sink UI.enabled bDisplayItem
    element elemCode # sink UI.enabled bDisplayItem

    element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)


    return (elem, emptyDataItem <$ eCreate)


emptyDataItem :: Item
emptyDataItem = Item.Item "" ""

dataItem :: Behavior (Maybe Item) -> UI ((Element, Element), Tidings Item)
dataItem bItem = do
    entry1 <- UI.entry $ Item.name . fromMaybe emptyDataItem <$> bItem
    entry2 <- UI.entry $ Item.code . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , Item.Item <$> UI.userText entry1 <*> UI.userText entry2
        )
