{-# LANGUAGE RecursiveDo #-}
module Loan.CreateNormal where

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

data CreateNormalEntry = CreateNormalEntry
    { _elementCE :: Element
    , _eConfirmLoan :: Event Loan

    -- TIDING
    , _modalStateCE :: Tidings Bool
    , _itemFilterCE :: Tidings String
    , _itemSelectionCE :: Tidings (Maybe DatabaseKey)
    }

instance Widget CreateNormalEntry where
    getElement = _elementCE


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m CreateNormalEntry
setup window = mdo

    (itemView, filterItem, listBoxItem) <- W.mkItemNormalBox

    (createBtnView, tCreate) <- W.mkLoanNormalBtn

    _elementCE               <-
        mkContainer
        $  [element itemView]
        ++ (fmap element createBtnView)


    -- Events and behaviors
    let eModalState = rumors tCreate

    bFilterEntryItem <- asks Env.bCreateLoanNormalFilterItem

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionItem = rumors $ UI.userSelection listBoxItem


    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken
    bSelectionItem  <- asks Env.bCreateLoanNormalSelectionItem

    bDisplayItem    <- displayItem
    bLookupItem     <- lookupItem
    bLookupLoan     <- lookupLoan
    bSelectedItem   <- selectedCreateLoanNormalItem
    bShowItem       <- showItem


    bLoanItemId     <- loanItemId


    bCreateLoan <- createLoanNormal


    let _itemSelectionCE = tidings bSelectionItem $ Unsafe.head <$> unions
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

        _itemFilterCE = tidings bFilterEntryItem $ Unsafe.head <$> unions
            [rumors $ UI.userText filterItem, "" <$ eModalState]

        _eConfirmLoan = filterJust $ bCreateLoan <@ filterJust
            ((\x -> if x then Nothing else Just ()) <$> eModalState)

        _modalStateCE = tCreate


    return CreateNormalEntry { .. }


