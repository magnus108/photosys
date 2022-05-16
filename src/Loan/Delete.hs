{-# LANGUAGE RecursiveDo #-}
module Loan.Delete where

import qualified Loan.Predicates as P
import           Utils.Utils
import           Data.Aeson

import Data.Functor.Contravariant.Divisible
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import qualified Modal
import qualified Counter
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
import           Layout
import           Behaviors
import           Loan.Behaviors



data DeleteEntry = DeleteEntry
    { _elementDE :: Element
    , _eDeleteLoan :: Event DatabaseKey
    , _userFilterDE    :: Tidings String
    , _itemFilterDE :: Tidings String
    , _userSelectionDE :: Tidings (Maybe DatabaseKey)
    , _itemSelectionDE :: Tidings (Maybe DatabaseKey)
    }

instance Widget DeleteEntry where getElement = _elementDE

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m DeleteEntry
setup window = mdo

    -- GUI elements
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


    (modalView , modal) <- mkModal bActiveModal bDisplayItemSelected


    (deleteBtn, deleteBtnView) <- mkButton "Aflever"


    -- GUI layout
    _elementDE <- mkContainer
        [ element userView
        , element itemView
        , element deleteBtnView
        , element modalView
        ]


    -- Events and behaviors

    let eDelete = UI.click deleteBtn

    bFilterEntryUser <- asks Env.bDeleteLoanFilterUser
    bFilterEntryItem <- asks Env.bDeleteLoanFilterItem


    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem

    let eModal = rumors $ Modal._stateModal modal

    bActiveModal <- stepper False $ Unsafe.head <$> unions [True <$ eDelete, eModal]


    bSelectionUser  <- asks Env.bDeleteLoanSelectionUser
    bSelectionItem  <- asks Env.bDeleteLoanSelectionItem
    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken


    bLookupUser     <- lookupUser
    bLookupItem     <- lookupItem
    bLookupLoan     <- lookupLoan

    bLoanItemId <- loanItemId
    bLoanUserId <- loanUserId

    bShowUser <- showUser

    bSelectedUser <- selectedUserDelete
    bSelectedItem <- selectedItemDelete

    bDisplayUserName <- displayUser
    bShowItem <- showItem
    bDisplayItem <- displayItem
    bDisplayItemSelected <- showItemDelete

    let itemFilter env = P.compareMaybe (P.selectionItem env) <> (contramap (P.showItem env) (Predicate (P.filterItem env)))
    let userFilter env = P.compareMaybe (P.selectionUser env) <> (contramap (P.showUser env) (Predicate (P.filterUser env)))
    let loanFilter env = getPredicate $ contramap (P.lookupLoan env) $
                                P.chooseMaybe $
                                    divide (\l -> (Loan.item l, Loan.user l))
                                                    (itemFilter env)
                                                    (userFilter env)

    let deleteFilter = P.DeleteLoanFilter <$> bLookupLoan <*> bSelectionUser <*> bSelectionItem <*> bFilterUser <*> bShowUser <*> bShowItem
    let bFilter = deleteFilter <*> bFilterItem

    let bSearchLoans = (\env -> filter (loanFilter env) . keys) <$> bFilter <*> bDatabaseLoan

    let bListBoxUsers' = (\f xs -> fmap Loan.user $ catMaybes $ fmap f xs) <$> bLookupLoan <*> bSearchLoans
    let bListBoxUsers = (\xs -> filter (\x -> List.elem x xs) . keys) <$> bListBoxUsers' <*> bDatabaseUser
    let bListBoxItems' = (\f xs -> fmap Loan.item $ catMaybes $ fmap f xs)<$> bLookupLoan <*> bSearchLoans
    let bListBoxItems = (\xs -> filter (\x -> List.elem x xs) . keys) <$> bListBoxItems' <*> bDatabaseItem


    let bSelectedLoan = listToFirst <$> bSearchLoans
        hasSelectedLoan = isJust <$> bSelectedLoan

    liftUI $ element deleteBtn # sink UI.enabled hasSelectedLoan

    let _userSelectionDE = tidings bSelectionUser $ Unsafe.head <$> unions
            [ eSelectionUser
            , (\items a b c d e f p -> case filter (loanFilter (P.DeleteLoanFilter a b c d e f p)) (keys items) of
                  (x : []) -> fmap Loan.user (a x)
                  (xs    ) -> b >>= \z -> if (loanFilter (P.DeleteLoanFilter a b c d e f p)) z then fmap Loan.user (a z) else Nothing
              )
            <$> bDatabaseLoan
            <*> bLookupLoan
            <*> bSelectionUser
            <*> bSelectionItem
            <*> bFilterUser
            <*> bShowUser
            <*> bShowItem
            <@> eFilterUser
            , Nothing <$ eModal
            ]

        _itemSelectionDE = tidings bSelectionItem $ Unsafe.head <$> unions
            [ eSelectionItem
            , (\items a b c d e f p -> case filter (loanFilter (P.DeleteLoanFilter a b c d e f p)) (keys items) of
                  (x : []) -> fmap Loan.item (a x)
                  (xs    ) -> b >>= \z -> if (loanFilter (P.DeleteLoanFilter a b c d e f p)) z then fmap Loan.item (a z) else Nothing
              )
            <$> bDatabaseLoan
            <*> bLookupLoan
            <*> bSelectionUser
            <*> bSelectionItem
            <*> bFilterUser
            <*> bShowUser
            <*> bShowItem
            <@> eFilterItem
            , Nothing <$ eModal
            ]

        _userFilterDE =
            tidings bFilterEntryUser $ Unsafe.head <$> unions
                [rumors $ UI.userText filterUser, "" <$ eModal]

        _itemFilterDE = tidings bFilterEntryItem $ Unsafe.head <$> unions
            [rumors $ UI.userText filterItem, "" <$ eModal]

        _eDeleteLoan = filterJust $ bSelectedLoan <@ eModal

    return DeleteEntry { .. }



listToFirst :: [a] -> Maybe a
listToFirst (x:[]) = Just x
listToFirst _ = Nothing


