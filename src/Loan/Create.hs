{-# LANGUAGE RecursiveDo #-}
module Loan.Create where

import qualified Loan.Widgets                  as W

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

data CreateEntry = CreateEntry
    { _elementCE :: Element
    , _eConfirmLoan :: Event Loan

    -- TIDING
    , _modalStateCE :: Tidings Bool
    , _userFilterCE    :: Tidings String
    , _itemFilterCE :: Tidings String
    , _userSelectionCE :: Tidings (Maybe DatabaseKey)
    , _itemSelectionCE :: Tidings (Maybe DatabaseKey)
    }

instance Widget CreateEntry where
    getElement = _elementCE


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m CreateEntry
setup window = mdo

    (userView, filterUser, listBoxUser) <- W.mkUserBox

    (itemView, filterItem, listBoxItem) <- W.mkItemBox

    (createBtnView, tCreate) <- W.mkLoanBtn

    infoElem                 <- W.info

    _elementCE               <-
        mkContainer
        $  [element userView, element itemView]
        ++ (fmap element createBtnView)
        ++ [element infoElem]


    -- Events and behaviors
    let eModalState = rumors tCreate

    bFilterEntryUser <- asks Env.bCreateLoanFilterUser
    bFilterEntryItem <- asks Env.bCreateLoanFilterItem


    let tFilterUser = isInfixOf <$> UI.userText filterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem


    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken
    bSelectionItem  <- asks Env.bCreateLoanSelectionItem
    bSelectionUser  <- asks Env.bCreateLoanSelectionUser

    bDisplayItem    <- displayItem
    bLookupUser     <- lookupUser
    bLookupItem     <- lookupItem
    bLookupLoan     <- lookupLoan
    bSelectedItem   <- selectedCreateLoanItem
    bShowUser       <- showUser
    bShowItem       <- showItem

    bDisplayUser    <- displayUser

    bLoanItemId     <- loanItemId
    bLoanUserId     <- loanUserId

    bListBoxUsers   <- createListBoxUsers


    {-
    let bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItemId <*> bDatabaseLoan

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p q show ->
                    filter (flip List.notElem q) . filter (p . show) . keys
                )
                <$> bFilterItem
                <*> bItemsWithLoan
                <*> bShowItem
                <*> bDatabaseItem
-}

    bCreateLoan <- createLoan


    let _userSelectionCE = tidings bSelectionUser $ Unsafe.head <$> unions
            [ eSelectionUser
            , (\b s users p -> case filter (p . s) (keys users) of
                  (x : []) -> Just x
                  (xs    ) -> b >>= \a -> if p (s a) then Just a else Nothing
              )
            <$> bSelectionUser
            <*> bShowUser
            <*> bDatabaseUser
            <@> eFilterUser
            ]

        _itemSelectionCE = tidings bSelectionItem $ Unsafe.head <$> unions
            [ eSelectionItem
            , (\b s items p -> case filter (p . s) (keys items) of
                  (x : []) -> Just x
                  (xs    ) -> b >>= \a -> if p (s a) then Just a else Nothing
              )
            <$> bSelectionItem
            <*> bShowItem
            <*> bDatabaseItem
            <@> eFilterItem
            , (\i x -> if x then i else Nothing)
            <$> bSelectionItem
            <@> eModalState
            ]

        _userFilterCE = tidings bFilterEntryUser $ Unsafe.head <$> unions
            [rumors $ UI.userText filterUser, "" <$ eModalState]

        _itemFilterCE = tidings bFilterEntryItem $ Unsafe.head <$> unions
            [rumors $ UI.userText filterItem, "" <$ eModalState]

        _eConfirmLoan = filterJust $ bCreateLoan <@ filterJust
            ((\x -> if x then Nothing else Just ()) <$> eModalState)

        _modalStateCE = tCreate


    return CreateEntry { .. }


