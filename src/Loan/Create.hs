{-# LANGUAGE RecursiveDo #-}
module Loan.Create where

import qualified Loan.Widgets as W

import           Data.Time
import qualified Modal

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
import           Loan.Behaviors
import           Layout
import           Utils.Utils

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Loan, Tidings (Maybe DatabaseKey))
setup window = mdo

    (userView, filterUser, listBoxUser) <- mkSearchEntry
                                                                                        bListBoxUsers
                                                                                        bSelectionUser
                                                                                        bDisplayUserName
                                                                                        bFilterEntryUser

    (itemView, filterItem, listBoxItem) <- mkSearchEntry
                                                                                        bListBoxItems
                                                                                        bSelectionItem
                                                                                        bDisplayItem
                                                                                        bFilterEntryItem

    (createBtn, createBtnView) <- mkButton "Lån"

    (modalView, modal) <- mkModal bActiveModal [UI.span # sink text ((maybe "" Item.name) <$> bSelectedItem)]

    infoElem <- W.info

    elem <- mkContainer
        [ element userView
        , element itemView
        , element createBtnView
        , element modalView
        , element infoElem
        ]


    -- Events and behaviors
    let eModal = rumors $ Modal._stateModal modal
    let eCreate        = UI.click createBtn
    bFilterEntryUser <- stepper "" $ Unsafe.head <$> unions [rumors $ UI.userText filterUser, "" <$ eModal]
    bFilterEntryItem <- stepper "" $ Unsafe.head <$> unions [rumors $ UI.userText filterItem, "" <$ eModal]


    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem


    bActiveModal <- stepper False $ Unsafe.head <$> unions [True <$ eCreate, eModal]


    bSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionUser
        , (\b s users p -> case filter (p . s) (keys users) of
                                (x:[]) -> Just x
                                (xs) -> b >>= \a -> if p (s a) then Just a else Nothing
          )
        <$> bSelectionUser
        <*> bShowUser
        <*> bDatabaseUser
        <@> eFilterUser
        ]


    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken
    bSelectionItem  <- asks Env.bCreateSelectionItem

    bDisplayItem <- displayItem
    bLookupUser     <- lookupUser
    bLookupItem     <- lookupItem
    bLookupLoan     <- lookupLoan
    bSelectedItem   <- selectedCreateLoanItem

    let bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectionUser

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



    let bCreateLoan :: Behavior (Maybe Loan)
        bCreateLoan = liftA2 Loan.Loan <$> bSelectionItem <*> bSelectionUser

        hasUserSelected :: Behavior Bool
        hasUserSelected = isJust <$> bSelectionUser

    hasItemSelected <- hasSelectedCreateLoanItem

    liftUI $ element createBtn # sink UI.enabled
                                      (hasUserSelected <&&> hasItemSelected)



    return
        ( elem
        , filterJust $ bCreateLoan <@ eModal
        , tidings
            bSelectionItem
            (Unsafe.head <$> unions
                [ eSelectionItem
                , (\b s items p -> case filter (p . s) (keys items) of
                                (x:[]) -> Just x
                                (xs) -> b >>= \a -> if p (s a) then Just a else Nothing
                )
                <$> bSelectionItem
                <*> bShowItem
                <*> bDatabaseItem
                <@> eFilterItem
                , Nothing <$ eModal
                ]
            )
        )
