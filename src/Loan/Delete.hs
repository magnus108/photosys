{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
module Loan.Delete where

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
    filterUser  <- UI.entry bFilterEntryUser
    listBoxUser <- UI.listBox bListBoxUsers bSelectionUser bDisplayUserName

    filterItem  <- UI.entry bFilterEntryItem
    listBoxItem <- UI.listBox bListBoxItems bSelectionItem bDisplayItemName

    deleteBtn   <- UI.button #+ [string "Aflever"]

    -- GUI layout
    searchUser  <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterUser #. "input" # set (attr "placeholder")
                                                    "Fx Anders Andersen"
              ]
           ]

    dropdownUser <-
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBoxUser # set (attr "size") "5" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    searchItem <-
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
               #+ [UI.div #. "box" #+ [string "Aflevering godkendt"]]
               , element closeBtn
               ]

    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchUser
                , element dropdownUser
                , element searchItem
                , element dropdownItem
                , element deleteBtn'
                , element modal
                ]
           ]


    -- Events and behaviors
    bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem


    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem
        eDelete        = UI.click deleteBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose]


    bSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionUser
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionUser
        <*> bShowUser
        <@> eFilterUser
        ]
    
    let bHelpMe :: Behavior
                (Maybe DatabaseKey -> Maybe DatabaseKey -> Maybe DatabaseKey)
        bHelpMe = do
            users <- bLoanUsers
            items <- bLoanItems
            pure $ \itemKey userKey ->
                if (List.elem itemKey items) && (List.elem userKey users)
                    then itemKey
                    else Nothing

        coco :: Event (Maybe DatabaseKey)
        coco = bHelpMe <*> bSelectionItem <@> eSelectionUser

    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowItem
        <@> eFilterItem
        , coco
        ]



    let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip lookup <$> bDatabaseLoan

        bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bLoanUser :: Behavior (DatabaseKey -> Maybe Int)
        bLoanUser = (fmap Loan.user .) <$> bLookupLoan

        bLoanKeys :: Behavior [DatabaseKey]
        bLoanKeys = keys <$> bDatabaseLoan

        bLoanItems :: Behavior [Maybe Int]
        bLoanItems = fmap <$> bLoanItem <*> bLoanKeys

        bLoanUsers :: Behavior [Maybe Int]
        bLoanUsers = fmap <$> bLoanUser <*> bLoanKeys


        bLoanItemsFilter :: Behavior (Int -> Bool)
        bLoanItemsFilter = flip List.elem . catMaybes <$> bLoanItems


        bLoanUserFilter :: Behavior (DatabaseKey -> Bool)
        bLoanUserFilter = do
            lookup       <- bLookupLoan
            selectedUser <- bSelectionUser
            pure
                $ \k ->
                      (selectedUser == Nothing)
                          || (fmap Loan.user (lookup k) == selectedUser)



        bFilterItemLoan :: Behavior [DatabaseKey]
        bFilterItemLoan =
            (\p q -> filter q . filter p . keys)
                <$> bLoanItemsFilter
                <*> bLoanUserFilter
                <*> bDatabaseItem


        bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectionUser

        bSelectedItem :: Behavior (Maybe Item)
        bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

        bShowUser :: Behavior (DatabaseKey -> String)
        bShowUser = (maybe "" User.name .) <$> bLookupUser

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.name .) <$> bLookupItem

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bListBoxUsers :: Behavior [DatabaseKey]
        bListBoxUsers =
            (\p show -> filter (p . show) . keys)
                <$> bFilterUser
                <*> bShowUser
                <*> bDatabaseUser

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show))
                <$> bFilterItem
                <*> bShowItem
                <*> bFilterItemLoan

    let hasUserSelected :: Behavior Bool
        hasUserSelected = isJust <$> bSelectionUser

        hasItemSelected :: Behavior Bool
        hasItemSelected = isJust <$> bSelectionItem

    element deleteBtn # sink UI.enabled (hasUserSelected <&&> hasItemSelected)
    element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    let bDelete = liftA2 Loan.Loan <$> bSelectionItem <*> bSelectionUser

    let bLoans :: Behavior [(DatabaseKey, Loan)]
        bLoans = toPairs <$> bDatabaseLoan

    let bSelectedLoan :: Behavior (Maybe DatabaseKey)
        bSelectedLoan = do
            loans <- bLoans
            loan <- bDelete
            pure $ fst <$> find (\(k,v) -> Just v == loan) loans

    return (elem, filterJust $ bSelectedLoan <@ eDelete)
