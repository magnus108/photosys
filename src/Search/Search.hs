{-# LANGUAGE RecursiveDo #-}
module Search.Search where

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
import           Layout
import           Behaviors
import Utils.Utils


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m Element
setup window = mdo
    -- GUI elements
    (filterUser , searchUser  ) <- mkSearch bFilterEntryUser
    (listBoxUser, dropdownUser) <- mkListBox bListBoxUsers
                                             bSelectionUser
                                             bDisplayUserName
    counterUser                 <- mkCounter bListBoxUsers

    (filterItem , searchItem  ) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems
                                             bSelectionItem
                                             bDisplayItemName
    counterItem <- mkCounter bListBoxItems

    -- sorta hack
    infoSerie   <-
        liftUI
        $  UI.div
        #+ [ string "Serie: "
           , UI.span
               # sink child (fmap <$> bDisplayItemSerie <*> bSelectionItem)
           ]
    infoPrice <-
        liftUI
        $  UI.div
        #+ [ string "Pris: "
           , UI.span
               # sink child (fmap <$> bDisplayItemPrice <*> bSelectionItem)
           ]
    infoVendor <-
        liftUI
        $  UI.div
        #+ [ string "Forhandler: "
           , UI.span
               # sink child (fmap <$> bDisplayItemVendor <*> bSelectionItem)
           ]

    infoInvoiceNumber <-
        liftUI
        $  UI.div
        #+ [ string "Fatura nr: "
           , UI.span # sink
               child
               (fmap <$> bDisplayItemInvoiceNumber <*> bSelectionItem)
           ]
    infoDateOfPurchase <-
        liftUI
        $  UI.div
        #+ [ string "Købsdato: "
           , UI.span # sink
               child
               (fmap <$> bDisplayItemDateOfPurchase <*> bSelectionItem)
           ]
    infoNote <-
        liftUI
        $  UI.div
        #+ [ string "Note: "
           , UI.span # sink child (fmap <$> bDisplayItemNote <*> bSelectionItem)
           ]

    infoElem <- liftUI $ UI.div # sink children bInfo
    let info =
            [ infoSerie
            , infoPrice
            , infoVendor
            , infoInvoiceNumber
            , infoDateOfPurchase
            , infoNote
            ]
        bInfo = (\b -> if b then info else []) <$> bHasSelectedItem
    -- sorta hack

    elem <- mkContainer
        [ element searchUser
        , element dropdownUser
        , element counterUser
        , element searchItem
        , element dropdownItem
        , element counterItem
        , element infoElem
        ]


    -- Events and behaviors
    bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem


    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem


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
        ]

    bDatabaseLoan    <- asks Env.bDatabaseLoan
    bDatabaseUser    <- asks Env.bDatabaseUser
    bDatabaseItem    <- asks Env.bDatabaseItem
    bDatabaseToken   <- asks Env.bDatabaseToken
    bSelectionToken  <- asks Env.bSelectionToken
    bDatabaseHistory <- asks Env.bDatabaseHistory


    bLookupUser <- lookupUser
    bLookupLoan <- lookupLoan


    let bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bLoanUser :: Behavior (DatabaseKey -> Maybe Int)
        bLoanUser = (fmap Loan.user .) <$> bLookupLoan

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

        bShowItemSerie :: Behavior (DatabaseKey -> String)
        bShowItemSerie = (maybe "" Item.serie .) <$> bLookupItem
        bDisplayItemSerie :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemSerie = (UI.string .) <$> bShowItemSerie

        bShowItemPrice :: Behavior (DatabaseKey -> String)
        bShowItemPrice = (maybe "" Item.price .) <$> bLookupItem
        bDisplayItemPrice :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemPrice = (UI.string .) <$> bShowItemPrice

        bShowItemInvoiceNumber :: Behavior (DatabaseKey -> String)
        bShowItemInvoiceNumber =
            (maybe "" Item.invoiceNumber .) <$> bLookupItem
        bDisplayItemInvoiceNumber :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemInvoiceNumber = (UI.string .) <$> bShowItemInvoiceNumber

        bShowItemDateOfPurchase :: Behavior (DatabaseKey -> String)
        bShowItemDateOfPurchase =
            (maybe "" Item.dateOfPurchase .) <$> bLookupItem
        bDisplayItemDateOfPurchase :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemDateOfPurchase = (UI.string .) <$> bShowItemDateOfPurchase

        bShowItemNote :: Behavior (DatabaseKey -> String)
        bShowItemNote = (maybe "" Item.note .) <$> bLookupItem
        bDisplayItemNote :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemNote = (UI.string .) <$> bShowItemNote

        bShowItemVendor :: Behavior (DatabaseKey -> String)
        bShowItemVendor = (maybe "" Item.vendor .) <$> bLookupItem
        bDisplayItemVendor :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemVendor = (UI.string .) <$> bShowItemVendor

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bListBoxUsers :: Behavior [DatabaseKey]
        bListBoxUsers =
            (\p q r show ->
                    filter (flip List.elem r)
                        . filter (flip List.elem q)
                        . filter (p . show)
                        . keys
                )
                <$> bFilterUser
                <*> bUsersWithLoan
                <*> bSelectionUsers
                <*> bShowUser
                <*> bDatabaseUser


        bUsersWithLoan :: Behavior [DatabaseKey]
        bUsersWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanUser <*> bDatabaseLoan

        bSelectionUsers :: Behavior [DatabaseKey]
        bSelectionUsers =
            (\i lookupItem lookupUser ->
                    catMaybes
                        . fmap lookupUser
                        . filter ((\x -> i == Nothing || i == x) . lookupItem)
                        . keys
                )
                <$> bSelectionItem
                <*> bLoanItem
                <*> bLoanUser
                <*> bDatabaseLoan

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p q r show ->
                    filter (flip List.elem r)
                        . filter (flip List.elem q)
                        . filter (p . show)
                        . keys
                )
                <$> bFilterItem
                <*> bItemsWithLoan
                <*> bSelectionItems
                <*> bShowItem
                <*> bDatabaseItem


        bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItem <*> bDatabaseLoan

        bSelectionItems :: Behavior [DatabaseKey]
        bSelectionItems =
            (\i lookupUser lookupItem ->
                    catMaybes
                        . fmap lookupItem
                        . filter ((\x -> i == Nothing || i == x) . lookupUser)
                        . keys
                )
                <$> bSelectionUser
                <*> bLoanUser
                <*> bLoanItem
                <*> bDatabaseLoan

    let bSelectedLoan :: Behavior (Maybe DatabaseKey)
        bSelectedLoan =
            (\item user lookup ->
                    find
                            ( (\x ->
                                  ((Loan.item <$> x) == item)
                                      && ((Loan.user <$> x) == user)
                              )
                            . lookup
                            )
                        . keys
                )
                <$> bSelectionItem
                <*> bSelectionUser
                <*> bLookupLoan
                <*> bDatabaseLoan

    let bHasSelectedItem :: Behavior Bool
        bHasSelectedItem =
            (\x xs -> case x of
                    Nothing -> False
                    Just y  -> List.elem y xs
                )
                <$> bSelectionItem
                <*> bListBoxItems

    return elem


child = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (catMaybes [i])
