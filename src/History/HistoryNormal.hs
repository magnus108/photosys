{-# LANGUAGE RecursiveDo #-}
module History.HistoryNormal where

import           Data.Time

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           History                        ( History )
import qualified History
import           Token                          ( Token )
import qualified Token

import           Loan                           ( Loan )
import qualified Loan
import           Time                           ( Time )
import qualified Time
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
import           Layout


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m Element
setup window = mdo

    -- GUI elements

    (filterItem, searchItem) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems'' bSelectionItem bDisplayItemName
    counterItem <- liftUI $ Counter.counter bListBoxItems''

    (filterLoan, searchLoan) <- mkSearch bFilterEntryLoan
    (listBoxLoan, dropdownLoan) <- mkListBox bListBoxLoans'' bSelectionLoan bDisplayLoanTime
    counterLoan <- liftUI $ Counter.counter bListBoxLoans''

    -- GUI layout
    elem <- mkContainer
        [ element searchItem
        , element dropdownItem
        , element counterItem
        , element searchLoan
        , element dropdownLoan
        , element counterLoan
        ]


    -- Events and behaviors
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem
    bFilterEntryLoan <- stepper "" . rumors $ UI.userText filterLoan

    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)


    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let tFilterLoan = isInfixOf <$> UI.userText filterLoan
        bFilterLoan = facts tFilterLoan
        eFilterLoan = rumors tFilterLoan

    let eSelectionItem = rumors $ UI.userSelection listBoxItem
        eSelectionLoan = rumors $ UI.userSelection listBoxLoan


    let bSelectionUser = bSelectedTokenId

    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowItem
        <@> eFilterItem
        ]

    bSelectionLoan <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionLoan
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionLoan
        <*> bShowLoan
        <@> eFilterLoan
        ]

    bDatabaseLoan    <- asks Env.bDatabaseLoan
    bDatabaseUser    <- asks Env.bDatabaseUser
    bDatabaseItem    <- asks Env.bDatabaseItem
    bDatabaseToken   <- asks Env.bDatabaseToken
    bSelectionToken  <- asks Env.bSelectionToken
    bDatabaseHistory <- asks Env.bDatabaseHistory


    let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan =
            (\x y -> fmap History.loan (lookup y x)) <$> bDatabaseHistory

        bLookupHistory :: Behavior (DatabaseKey -> Maybe History)
        bLookupHistory = flip lookup <$> bDatabaseHistory

        bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectionUser

        bSelectedItem :: Behavior (Maybe Item)
        bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

        bSelectedLoan :: Behavior (Maybe Loan)
        bSelectedLoan = (=<<) <$> bLookupLoan <*> bSelectionLoan

        bShowUser :: Behavior (DatabaseKey -> String)
        bShowUser = (maybe "" User.name .) <$> bLookupUser

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.showItem .) <$> bLookupItem

        bShowLoan :: Behavior (DatabaseKey -> String)
        bShowLoan =
            (maybe "" (Time.time . History.timestamp) .) <$> bLookupHistory

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bDisplayLoanTime :: Behavior (DatabaseKey -> UI Element)
        bDisplayLoanTime = (UI.string .) <$> bShowLoan

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bListBoxLoans :: Behavior [DatabaseKey]
        bListBoxLoans =
            (\p show -> filter (p . show) . keys)
                <$> bFilterLoan
                <*> bShowLoan
                <*> bDatabaseHistory

        bListBoxLoans' :: Behavior [DatabaseKey]
        bListBoxLoans' =
            (\mi lookup -> filter
                    ( (\ml ->
                          fromMaybe True (liftA2 (\l i -> Loan.item l == i) ml mi)
                      )
                    . lookup
                    )
                )
                <$> bSelectionItem
                <*> bLookupLoan
                <*> bListBoxLoans

        bListBoxLoans'' :: Behavior [DatabaseKey]
        bListBoxLoans'' =
            (\mu lookup -> filter
                    ( (\ml ->
                          fromMaybe True (liftA2 (\l u -> Loan.user l == u) ml mu)
                      )
                    . lookup
                    )
                )
                <$> bSelectionUser
                <*> bLookupLoan
                <*> bListBoxLoans'


        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilterItem
                <*> bShowItem
                <*> bDatabaseItem

        bListBoxItems' :: Behavior [DatabaseKey]
        bListBoxItems' =
            (\loan items -> filter
                    (\itemKey -> maybe True (\l -> Loan.item l == itemKey) loan)
                    items
                )
                <$> bSelectedLoan
                <*> bListBoxItems

        bListBoxItems'' :: Behavior [DatabaseKey]
        bListBoxItems'' =
            (\loans lookup ->
                    filter
                        (flip
                            List.elem
                            (catMaybes (fmap Loan.item <$> (lookup <$> loans)))
                        )
                )
                <$> bListBoxLoans''
                <*> bLookupLoan
                <*> bListBoxItems'

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken

    return elem
