module Loan.Widgets where

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
