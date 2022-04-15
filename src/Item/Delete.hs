{-# LANGUAGE RecursiveDo #-}
module Item.Delete where

import           Data.Aeson

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
    -> UI (Element, Event DatabaseKey)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem = mdo

    -- GUI elements
    filterItem  <- UI.entry bFilterEntryItem
    listBoxItem <- UI.listBox bListBoxItems bSelectionItem bDisplayItemName

    deleteBtn   <- UI.button #+ [string "Slet"]

    -- GUI layout
    searchItem  <-
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


    deleteBtn' <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element deleteBtn #. "button"]]


    closeBtn <- UI.button #. "modal-close is-large"

    modal    <-
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Sletning godkendt"]]
               , element closeBtn
               ]

    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchItem
                , element dropdownItem
                , element deleteBtn'
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
        eDelete        = UI.click deleteBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose]

    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , Nothing <$ eDelete
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowDataItem
        <@> eFilterItem
        ]


    let bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip lookup <$> bDatabaseLoan

        bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" Item.showItem .) <$> bLookupItem

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowDataItem

        bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = (\p q show -> filter (flip List.notElem q) . filter (p . show)  . keys)
                <$> bFilterItem
                <*> bItemsWithLoan
                <*> bShowDataItem
                <*> bDatabaseItem

        bSelectionDataItem :: Behavior (Maybe Item)
        bSelectionDataItem = (=<<) <$> bLookupItem <*> bSelectionItem


        bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItem <*> bDatabaseLoan


    let bHasSelectedItem :: Behavior Bool
        bHasSelectedItem = (\x xs -> case x of
                                       Nothing -> False
                                       Just y -> List.elem y xs
                           ) <$> bSelectionItem <*> bListBoxItems


    element deleteBtn # sink UI.enabled bHasSelectedItem
    element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    return (elem, filterJust $ bSelectionItem <@ eDelete)
