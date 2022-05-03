{-# LANGUAGE RecursiveDo #-}
module History.HistoryHandin where

import           Data.Time
import           Utils.Utils

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Time                           ( Time )
import qualified Time
import           HistoryHandin                  ( HistoryHandin )
import qualified HistoryHandin
import           History                        ( History )
import qualified History
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
import Layout
import Behaviors


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Tidings (Maybe DatabaseKey), Tidings (Maybe DatabaseKey), Tidings (Maybe DatabaseKey))
setup window = mdo

    -- GUI elements
    (filterUser, searchUser) <- mkSearch bFilterEntryUser
    (listBoxUser, dropdownUser) <- mkListBox bListBoxUsers'' bSelectionUser bDisplayUserName
    counterUser <- liftUI $ Counter.counter bListBoxUsers''

    (filterItem, searchItem) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems'' bSelectionItem bDisplayItemName
    counterItem <- liftUI $ Counter.counter bListBoxItems''

    (filterLoan, searchLoan) <- mkSearch bFilterEntryLoan
    (listBoxLoan, dropdownLoan) <- mkListBox bListBoxLoans'' bSelectionLoan bDisplayLoanTime
    counterLoan <- liftUI $ Counter.counter bListBoxLoans''

    isAdmin     <- liftUI $ UI.div

    -- GUI layout

    elem <- mkContainer
             [ element searchUser
                , element dropdownUser
                , element counterUser
                , element searchItem
                , element dropdownItem
                , element counterItem
                , element searchLoan
                , element dropdownLoan
                , element counterLoan
                , element isAdmin
                ]


    -- Events and behaviors
    bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem
    bFilterEntryLoan <- stepper "" . rumors $ UI.userText filterLoan

    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let tFilterLoan = isInfixOf <$> UI.userText filterLoan
        bFilterLoan = facts tFilterLoan
        eFilterLoan = rumors tFilterLoan

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem
        eSelectionLoan = rumors $ UI.userSelection listBoxLoan


    bSelectionItem <- asks Env.bHistoryHandinItem
    bSelectionUser <- asks Env.bHistoryHandinUser
    bSelectionLoan <- asks Env.bHistoryHandinLoan
    bDatabaseLoan          <- asks Env.bDatabaseLoan
    bDatabaseUser          <- asks Env.bDatabaseUser
    bDatabaseItem          <- asks Env.bDatabaseItem
    bDatabaseToken         <- asks Env.bDatabaseToken
    bSelectionToken        <- asks Env.bSelectionToken
    bDatabaseHistoryHandin <- asks Env.bDatabaseHistoryHandin

    bLookupUser <- lookupUser
    bLookupItem <- lookupItem
    bLookupHistoryHandin <- lookupHistoryHandin

    bSelectedLoan <- historyHandinLoan
    bSelectedUser <- historyHandinUser
    bSelectedItem <- historyHandinItem

    let bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = (fmap HistoryHandin.loan .) <$> bLookupHistoryHandin

        bShowLoan :: Behavior (DatabaseKey -> String)
        bShowLoan =
            (maybe "" (Time.time . HistoryHandin.timestamp) .) <$> bLookupHistoryHandin

        bShowUser :: Behavior (DatabaseKey -> String)
        bShowUser = (maybe "" User.name .) <$> bLookupUser

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.name .) <$> bLookupItem


        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bDisplayLoanTime :: Behavior (DatabaseKey -> UI Element)
        bDisplayLoanTime = (UI.string .) <$> bShowLoan

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bShowAdmin :: Behavior (DatabaseKey -> Maybe Int)
        bShowAdmin =
            (fmap (HistoryHandin.adminUser) .) <$> bLookupHistoryHandin

        bShowAdmin2 :: Behavior (Maybe User)
        bShowAdmin2 =
            (\f x y -> f =<< (x =<< y))
                <$> bLookupUser
                <*> bShowAdmin
                <*> bSelectionLoan


        bListBoxLoans :: Behavior [DatabaseKey]
        bListBoxLoans =
            (\p show -> filter (p . show) . keys)
                <$> bFilterLoan
                <*> bShowLoan
                <*> bDatabaseHistoryHandin

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

        bListBoxUsers :: Behavior [DatabaseKey]
        bListBoxUsers =
            (\p show -> filter (p . show) . keys)
                <$> bFilterUser
                <*> bShowUser
                <*> bDatabaseUser


        bListBoxUsers' :: Behavior [DatabaseKey]
        bListBoxUsers' =
            (\loan users -> filter
                    (\userKey -> maybe True (\l -> Loan.user l == userKey) loan)
                    users
                )
                <$> bSelectedLoan
                <*> bListBoxUsers

        bListBoxUsers'' :: Behavior [DatabaseKey]
        bListBoxUsers'' =
            (\loans lookup ->
                    filter
                        (flip
                            List.elem
                            (catMaybes (fmap Loan.user <$> (lookup <$> loans)))
                        )
                )
                <$> bListBoxLoans''
                <*> bLookupLoan
                <*> bListBoxUsers'


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

    let bIsAdminGUI =
            fmap (\x -> UI.string (User.name x) #. "tag is-dark is-large")
                <$> bShowAdmin2
    liftUI $ element isAdmin # sink items
                                    ((\x -> catMaybes [x]) <$> bIsAdminGUI)

    let tSelectionLoan = tidings bSelectionLoan $ Unsafe.head <$> unions
                                                        [ eSelectionLoan
                                                        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
                                                        <$> bSelectionLoan
                                                        <*> bShowLoan
                                                        <@> eFilterLoan
                                                        ]
    let tSelectionUser = tidings bSelectionUser $ Unsafe.head <$> unions
                                                        [ eSelectionUser
                                                        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
                                                        <$> bSelectionUser
                                                        <*> bShowUser
                                                        <@> eFilterUser
                                                        ]


    let tSelectionItem = tidings bSelectionItem $ Unsafe.head <$> unions
                                                        [ eSelectionItem
                                                        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
                                                        <$> bSelectionItem
                                                        <*> bShowItem
                                                        <@> eFilterItem
                                                        ]

    return (elem, tSelectionLoan, tSelectionUser, tSelectionItem)

items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
