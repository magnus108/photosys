{-# LANGUAGE RecursiveDo #-}
module Loan.CreateNormal where

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

import           Behaviors
import           Layout

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Loan)
setup window = mdo


    -- GUI elements
    (filterItem, searchItem) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems bSelectionItem bDisplayItemName
    (createBtn, createBtnView) <- mkButton "Lån"

    counter    <- mkCounter bListBoxItems
    loanInfo   <- liftUI $ UI.span

    -- GUI layout
    closeBtn <- liftUI $ UI.button #. "modal-close is-large"
    modal    <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-content"
           #+ [UI.div #. "box" #+ [string "Lån godkendt: ", element loanInfo]]
           , element closeBtn
           ]

    elem <-
        liftUI
        $  UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchItem
                , element dropdownItem
                , element createBtnView
                , element counter
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
        eCreate        = UI.click createBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]

    bLastLoanItem <- stepper Nothing $ Unsafe.head <$> unions
        [bSelectionItem <@ eCreate]


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


    bLookupUser     <- lookupUser
    bLookupLoan     <- lookupLoan
    bLookupItem     <- lookupItem
    bLookupToken    <- lookupToken
    bShowUser       <- showUser
    bSelectedToken  <- selectedToken
    bShowItem       <- showItem
    bShowItemCode   <- showItemCode

    --- Forkert men okay
    bSelectedItem   <- selectedItem bSelectionItem

    let bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

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
                <*> bShowItemCode
                <*> bDatabaseItem


        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken


    let bCreateLoan :: Behavior (Maybe Loan)
        bCreateLoan = liftA2 Loan.Loan <$> bSelectionItem <*> bSelectedTokenId
        -- <*> bSelectedTokenId

        hasUserSelected :: Behavior Bool
        hasUserSelected = isJust <$> bSelectedTokenId

        hasItemSelected :: Behavior Bool
        hasItemSelected = isJust <$> bSelectionItem

        bLastLoanItemItem :: Behavior (Maybe Item)
        bLastLoanItemItem = (=<<) <$> bLookupItem <*> bLastLoanItem


    liftUI $ element loanInfo # sink
        text
        ((maybe "" Item.name) <$> bLastLoanItemItem)

    liftUI $ element createBtn # sink UI.enabled
                                      (hasUserSelected <&&> hasItemSelected)

    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)


    return (elem, filterJust $ bCreateLoan <@ eCreate)
