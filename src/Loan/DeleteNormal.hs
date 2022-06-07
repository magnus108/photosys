{-# LANGUAGE RecursiveDo #-}
module Loan.DeleteNormal where

import qualified Loan.Predicates               as P
import           Token
import           Utils.Utils
import           Data.Aeson

import           Data.Functor.Contravariant.Divisible
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



data DeleteNormalEntry = DeleteNormalEntry
    { _elementDE :: Element
    , _eDeleteLoan :: Event DatabaseKey
    , _itemFilterDE :: Tidings String
    , _itemSelectionDE :: Tidings (Maybe DatabaseKey)
    }

instance Widget DeleteNormalEntry where
    getElement = _elementDE

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m DeleteNormalEntry
setup window = mdo

    -- GUI elements
    (itemView, filterItem, listBoxItem) <- mkSearchEntry bListBoxItems
                                                         bSelectionItem
                                                         bDisplayItem
                                                         bFilterEntryItem


    (modalView, modal) <- mkModal bActiveModal
                                  [UI.span # sink text bShowItemSelected]


    (deleteBtn, deleteBtnView) <- mkButton "Aflever"


    _elementDE                 <- mkContainer
        [ element itemView
        , element deleteBtnView
        , element modalView
        ]


    let eDelete = UI.click deleteBtn

    bFilterEntryItem <- asks Env.bDeleteLoanNormalFilterItem

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionItem = rumors $ UI.userSelection listBoxItem

    let eModal = rumors $ Modal._stateModal modal

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, eModal]


    bSelectionItem    <- asks Env.bDeleteLoanNormalSelectionItem
    bDatabaseLoan     <- asks Env.bDatabaseLoan
    bDatabaseUser     <- asks Env.bDatabaseUser
    bDatabaseItem     <- asks Env.bDatabaseItem
    bDatabaseToken    <- asks Env.bDatabaseToken
    bSelectionToken   <- asks Env.bSelectionToken


    bLookupUser       <- lookupUser
    bLookupItem       <- lookupItem
    bLookupLoan       <- lookupLoan

    bLoanItemId       <- loanItemId
    bLoanUserId       <- loanUserId

    bShowUser         <- showUser

    bSelectedUser     <- selectedUserDeleteNormal
    bSelectedItem     <- selectedItemDeleteNormal

    bDisplayUserName  <- displayUser
    bShowItem         <- showItem
    bDisplayItem      <- displayItem
    bShowItemSelected <- showItemDeleteNormal

    bSelectedToken <- selectedToken
    let bSelectionUser = chainedTo Token.tokenId <$> bSelectedToken

    let loanFilter = getPredicate . P.pLoanFilter

    let deleteFilter =
            P.DeleteLoanFilter
                <$> bLookupLoan
                <*> bSelectionUser
                <*> bSelectionItem
                <*> (pure (const True))
                <*> bShowUser
                <*> bShowItem




    let bFilter = deleteFilter <*> bFilterItem

    let bSearchLoans =
            (\env -> filter (loanFilter env) . keys)
                <$> bFilter
                <*> bDatabaseLoan

    let bListBoxItems' =
            (\f xs -> fmap Loan.item $ catMaybes $ fmap f xs)
                <$> bLookupLoan
                <*> bSearchLoans
    let bListBoxItems =
            (\xs -> filter (\x -> List.elem x xs) . keys)
                <$> bListBoxItems'
                <*> bDatabaseItem


    let bSelectedLoan   = listToFirst <$> bSearchLoans
        hasSelectedLoan = isJust <$> bSelectedLoan

    liftUI $ element deleteBtn # sink UI.enabled hasSelectedLoan
    liftUI $ element deleteBtn # sink sinkFocus hasSelectedLoan
    liftUI $ element filterItem # sink sinkFocus (not <$>bActiveModal)


    let _itemSelectionDE = tidings bSelectionItem $ Unsafe.head <$> unions
            [ eSelectionItem
            , (\items a b c d e f p ->
                  case
                          filter
                              (loanFilter (P.DeleteLoanFilter a b c d e f p))
                              (keys items)
                      of
                          (x : []) -> fmap Loan.item (a x)
                          (xs    ) -> b >>= \z ->
                              if (loanFilter (P.DeleteLoanFilter a b c d e f p))
                                  z
                              then
                                  fmap Loan.item (a z)
                              else
                                  Nothing
              )
            <$> bDatabaseLoan
            <*> bLookupLoan
            <*> bSelectionUser
            <*> bSelectionItem
            <*> (pure (const True))
            <*> bShowUser
            <*> bShowItem
            <@> eFilterItem
            , Nothing <$ eModal
            ]

        _itemFilterDE = tidings bFilterEntryItem $ Unsafe.head <$> unions
            [rumors $ UI.userText filterItem, "" <$ eModal]

        _eDeleteLoan = filterJust $ bSelectedLoan <@ eModal

    return DeleteNormalEntry { .. }



listToFirst :: [a] -> Maybe a
listToFirst (x : []) = Just x
listToFirst _        = Nothing


sinkFocus = mkWriteAttr $ \b x -> void $ do
    if b then UI.setFocus x else return ()
