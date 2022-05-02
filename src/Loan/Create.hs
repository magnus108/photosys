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
import           Behaviors
import           Layout
import           Utils.Utils

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Loan)
setup window = mdo

    -- GUI elements
    (filterUser , searchUser  ) <- mkSearch bFilterEntryUser
    (listBoxUser, dropdownUser) <- mkListBox bListBoxUsers
                                             bSelectionUser
                                             bDisplayUserName
    counterUser                 <- liftUI $ Counter.counter bListBoxUsers

    (filterItem , searchItem  ) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems
                                             bSelectionItem
                                             bDisplayItemName
    counterItem                <- liftUI $ Counter.counter bListBoxItems

    (createBtn, createBtnView) <- mkButton "Lån"

    loanInfo                   <- liftUI $ UI.span
    -- GUI layout
    closeBtn                   <- liftUI $ UI.button #. "modal-close is-large"
    modal                      <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-content"
           #+ [UI.div #. "box" #+ [string "Lån godkendt: ", element loanInfo]]
           , element closeBtn
           ]

    --- ugs
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
        , element createBtnView
        , element counterItem
        , element modal
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

    bLookupUser     <- lookupUser
    bLookupItem     <- lookupItem
    bLookupLoan     <- lookupLoan

    let bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

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

    liftUI $ element loanInfo # sink
        text
        ((maybe "" Item.name) <$> bLastLoanItemItem)
    liftUI $ element createBtn # sink UI.enabled
                                      (hasUserSelected <&&> hasItemSelected)
    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)


    return (elem, filterJust $ bCreateLoan <@ eCreate)

child = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (catMaybes [i])
