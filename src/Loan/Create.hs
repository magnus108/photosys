{-# LANGUAGE RecursiveDo #-}
module Loan.Create where

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
import           Layout
import           Utils.Utils

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Loan, Tidings (Maybe DatabaseKey))
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
    counterItem                <- mkCounter bListBoxItems

    (createBtn, createBtnView) <- mkButton "Lån"

    -- GUI layout
    (modalView, modal) <- mkModal bActiveModal [UI.span # sink text ((maybe "" Item.name) <$> bLastLoanItemItem)]

    --- ugs
    infoSerie <-
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
        , element createBtnView
        , element counterItem
        , element modalView
        , element infoElem
        ]


    -- Events and behaviors
    let eCreate        = UI.click createBtn
    bFilterEntryUser <- stepper "" $ Unsafe.head <$> unions [rumors $ UI.userText filterUser, "" <$ eCreate]
    bFilterEntryItem <- stepper "" $ Unsafe.head <$> unions [rumors $ UI.userText filterItem, "" <$ eCreate]



    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem

    let eModal = rumors $ Modal._stateModal modal

    bActiveModal <- stepper False $ Unsafe.head <$> unions [True <$ eCreate, eModal]


    bLastLoanItem <- stepper Nothing $ Unsafe.head <$> unions
        [bSelectionItem <@ eCreate]


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

        bLastLoanItemItem :: Behavior (Maybe Item)
        bLastLoanItemItem = (=<<) <$> bLookupItem <*> bLastLoanItem



    let bCreateLoan :: Behavior (Maybe Loan)
        bCreateLoan = liftA2 Loan.Loan <$> bSelectionItem <*> bSelectionUser
-- <*> bSelectedTokenId

        hasUserSelected :: Behavior Bool
        hasUserSelected = isJust <$> bSelectionUser

        hasItemSelected :: Behavior Bool
        hasItemSelected = isJust <$> bSelectionItem

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


    let bHasSelectedItem :: Behavior Bool
        bHasSelectedItem =
            (\x xs -> case x of
                    Nothing -> False
                    Just y  -> List.elem y xs
                )
                <$> bSelectionItem
                <*> bListBoxItems

    liftUI $ element createBtn # sink UI.enabled
                                      (hasUserSelected <&&> hasItemSelected)



    return
        ( elem
        , filterJust $ bCreateLoan <@ eCreate
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
                , Nothing <$ eCreate
                ]
            )
        )

child = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (catMaybes [i])
