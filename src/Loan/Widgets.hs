module Loan.Widgets where

import           Data.Time
import           Modal                          ( Modal )
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

infoSerie
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
infoSerie = do
    bSelectedItem <- selectedCreateLoanItem
    liftUI
        $  UI.div
        #+ [ string "Serie: "
           , UI.span # sink text ((maybe "" Item.serie) <$> bSelectedItem)
           ]

infoPrice
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
infoPrice = do
    bSelectedItem <- selectedCreateLoanItem
    liftUI
        $  UI.div
        #+ [ string "Pris: "
           , UI.span # sink text ((maybe "" Item.price) <$> bSelectedItem)
           ]

infoVendor
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
infoVendor = do
    bSelectedItem <- selectedCreateLoanItem
    liftUI
        $  UI.div
        #+ [ string "Forhandler: "
           , UI.span # sink text ((maybe "" Item.vendor) <$> bSelectedItem)
           ]

infoInvoiceNumber
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
infoInvoiceNumber = do
    bSelectedItem <- selectedCreateLoanItem
    liftUI
        $  UI.div
        #+ [ string "Fatura nr: "
           , UI.span
               # sink text ((maybe "" Item.invoiceNumber) <$> bSelectedItem)
           ]

infoDateOfPurchase
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
infoDateOfPurchase = do
    bSelectedItem <- selectedCreateLoanItem
    liftUI
        $  UI.div
        #+ [ string "Købsdato: "
           , UI.span
               # sink text ((maybe "" Item.dateOfPurchase) <$> bSelectedItem)
           ]


infoNote
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
infoNote = do
    bSelectedItem <- selectedCreateLoanItem
    liftUI
        $  UI.div
        #+ [ string "Note: "
           , UI.span # sink text ((maybe "" Item.note) <$> bSelectedItem)
           ]

info
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m Element
info = do
    infoSerie'          <- infoSerie
    infoPrice'          <- infoPrice
    infoVendor'         <- infoVendor
    infoInvoiceNumber'  <- infoInvoiceNumber
    infoDateOfPurchase' <- infoDateOfPurchase
    infoNote'           <- infoNote
    hasItemSelected     <- hasSelectedCreateLoanItem
    liftUI $ UI.div # sink
        children
        (   (\b -> if b
                then
                    [ infoSerie'
                    , infoPrice'
                    , infoVendor'
                    , infoInvoiceNumber'
                    , infoDateOfPurchase'
                    , infoNote'
                    ]
                else []
            )
        <$> hasItemSelected
        )


mkCreateLoanModal
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Element, Modal)
mkCreateLoanModal = do
    bActiveModal  <- asks Env.bCreateLoanModalState
    bSelectedItem <- selectedCreateLoanItem
    mkModal bActiveModal
            [UI.span # sink text ((maybe "" Item.name) <$> bSelectedItem)]

mkCreateLoanNormalModal
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Element, Modal)
mkCreateLoanNormalModal = do
    bActiveModal  <- asks Env.bCreateLoanNormalModalState
    bSelectedItem <- selectedCreateLoanNormalItem
    mkModal bActiveModal
            [UI.span # sink text ((maybe "" Item.name) <$> bSelectedItem)]


mkLoanBtn
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m ([Element], Tidings Bool)
mkLoanBtn = do
    (createBtn, createBtnView) <- mkButton "Lån"
    (modalView, modal        ) <- mkCreateLoanModal

    let eModalState = rumors $ Modal._stateModal modal
    let bModalState = facts $ Modal._stateModal modal
    let eCreateLoan = UI.click createBtn

    bActiveModal <- asks Env.bCreateLoanModalState

    let modalState = tidings bActiveModal $ Unsafe.head <$> unions
            [True <$ eCreateLoan, eModalState]

    bCanCreateLoan <- canCreateLoan
    liftUI $ element createBtn # sink UI.enabled bCanCreateLoan
    liftUI $ element createBtn # sink sinkFocus ((\x y -> x && not y) <$>bCanCreateLoan <*>bModalState)

    return ([createBtnView, modalView], modalState)

mkLoanNormalBtn
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m ([Element], Tidings Bool)
mkLoanNormalBtn = do
    (createBtn, createBtnView) <- mkButton "Lån"
    (modalView, modal        ) <- mkCreateLoanNormalModal

    let eModalState = rumors $ Modal._stateModal modal
    let bModalState = facts $ Modal._stateModal modal
    let eCreateLoan = UI.click createBtn

    bActiveModal <- asks Env.bCreateLoanNormalModalState

    let modalState = tidings bActiveModal $ Unsafe.head <$> unions
            [True <$ eCreateLoan, eModalState]

    bCanCreateLoan <- canCreateLoanNormal
    liftUI $ element createBtn # sink UI.enabled bCanCreateLoan
    liftUI $ element createBtn # sink sinkFocus ((\x y -> x && not y) <$>bCanCreateLoan <*>bModalState)

    return ([createBtnView, modalView], modalState)


sinkFocus = mkWriteAttr $ \b x -> void $ do
    if b then UI.setFocus x else return ()

mkUserBox
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Element, UI.TextEntry, UI.ListBox DatabaseKey)
mkUserBox = do
    bSelection   <- asks Env.bCreateLoanSelectionUser
    bFilterEntry <- asks Env.bCreateLoanFilterUser
    bDisplay     <- displayUser
    bListBox     <- createListBoxUsers
    mkSearchEntry bListBox bSelection bDisplay bFilterEntry


mkItemBox
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Element, UI.TextEntry, UI.ListBox DatabaseKey)
mkItemBox = do
    bSelection   <- asks Env.bCreateLoanSelectionItem
    bFilterEntry <- asks Env.bCreateLoanFilterItem
    bDisplay     <- displayItem
    bListBox     <- createListBoxItems

    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bLoanItemId     <- loanItemId

    let bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItemId <*> bDatabaseLoan

        bFilterItems  = (\p q -> filter (flip List.notElem q) p) <$> bListBox <*> bItemsWithLoan

    mkSearchEntry bFilterItems bSelection bDisplay bFilterEntry

mkItemNormalBox
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => m (Element, UI.TextEntry, UI.ListBox DatabaseKey)
mkItemNormalBox = do
    bSelection   <- asks Env.bCreateLoanNormalSelectionItem
    bFilterEntry <- asks Env.bCreateLoanNormalFilterItem
    bDisplay     <- displayItem
    bListBox     <- createListBoxItemsNormal

    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bLoanItemId     <- loanItemId

    let bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItemId <*> bDatabaseLoan

        bFilterItems  = (\p q -> filter (flip List.notElem q) p) <$> bListBox <*> bItemsWithLoan

    mkSearchEntry bFilterItems bSelection bDisplay bFilterEntry
