{-# LANGUAGE RecursiveDo #-}
module Loan.Create where

import           Data.Time

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
import qualified Counter

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Loan)
setup window = mdo

    -- GUI elements
    filterUser  <- liftUI $ UI.entry bFilterEntryUser
    listBoxUser <- liftUI $ UI.listBox bListBoxUsers bSelectionUser bDisplayUserName
    counterUser <- liftUI $ Counter.counter bListBoxUsers

    filterItem  <- liftUI $ UI.entry bFilterEntryItem
    listBoxItem <- liftUI $ UI.listBox bListBoxItems bSelectionItem bDisplayItemName
    counterItem <- liftUI $ Counter.counter bListBoxItems

    createBtn   <- liftUI $ UI.button #+ [string "Lån"]

    loanInfo <- liftUI $ UI.span
    -- GUI layout
    searchUser  <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterUser #. "input" # set (attr "placeholder")
                                                    "Fx Anders Andersen"
              ]
           ]

    dropdownUser <- liftUI $
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

    searchItem <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterItem #. "input" # set (attr "placeholder")
                                                    "Fx Kamera"
              ]
           ]

    dropdownItem <- liftUI $
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


    createBtn' <- liftUI $
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]



    closeBtn <- liftUI $ UI.button #. "modal-close is-large"
    modal    <- liftUI $
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Lån godkendt: ", element loanInfo]]
               , element closeBtn
               ]

    elem <- liftUI $
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchUser
                , element dropdownUser
                , element counterUser
                , element searchItem
                , element dropdownItem
                , element createBtn'
                , element counterItem
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
        eCreate        = UI.click createBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]

    bLastLoanItem <- stepper Nothing $ Unsafe.head <$> unions
        [bSelectionItem <@ eCreate]


    bSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionUser
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionUser
        <*> bShowUser
        <@> eFilterUser
        ]

    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowItem
        <@> eFilterItem
        , Nothing <$ eCreate
        ]

    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken

    let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip lookup <$> bDatabaseLoan

        bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectionUser

        bSelectedItem :: Behavior (Maybe Item)
        bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

        bShowUser :: Behavior (DatabaseKey -> String)
        bShowUser = (maybe "" User.name .) <$> bLookupUser

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.showItem .) <$> bLookupItem

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


        bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItem <*> bDatabaseLoan

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p q show ->
                    filter (flip List.notElem q) . filter (p . show) . keys
                )
                <$> bFilterItem
                <*> bItemsWithLoan
                <*> bShowItem
                <*> bDatabaseItem

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken

        bLastLoanItemItem :: Behavior (Maybe Item)
        bLastLoanItemItem = (=<<) <$> bLookupItem <*> bLastLoanItem



    let bCreateLoan :: Behavior (Maybe Loan)
        bCreateLoan =
            liftA2 Loan.Loan
                <$> bSelectionItem
                <*> bSelectionUser
                -- <*> bSelectedTokenId

        hasUserSelected :: Behavior Bool
        hasUserSelected = isJust <$> bSelectionUser

        hasItemSelected :: Behavior Bool
        hasItemSelected = isJust <$> bSelectionItem

    liftUI $ element loanInfo # sink text ((maybe "" Item.name) <$> bLastLoanItemItem)
    liftUI $ element createBtn # sink UI.enabled (hasUserSelected <&&> hasItemSelected)
    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)


    return (elem, filterJust $ bCreateLoan <@ eCreate)
