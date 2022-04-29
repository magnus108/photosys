{-# LANGUAGE RecursiveDo #-}
module User.Delete where

import           Data.Aeson

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
import           Utils
import qualified Modal

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event DatabaseKey)
setup window = mdo
    -- GUI elements
    (filterUser , searchUser  ) <- mkSearch bFilterEntryUser
    (listBoxUser, dropdownUser) <- mkListBox bListBoxUsers
                                             bSelectionUser
                                             bDisplayUserName
    (deleteBtn, deleteBtnView)         <- mkButton "Slet"

    counter                            <- liftUI $ Counter.counter bListBoxUsers
    (realDeleteBtn, realDeleteBtnView) <- mkButton "Sikker på slet?"

    -- GUI layout
    modal <- liftUI $ Modal.modal (element realDeleteBtnView) bActiveModal

    elem                               <- mkContainer
        [ element searchUser
        , element dropdownUser
        , element deleteBtnView
        , element counter
        , element modal
        ]


    -- Events and behaviors
    bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser

    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eDelete        = UI.click deleteBtn
        eRealDelete    = UI.click realDeleteBtn

        eModal         = rumors $ Modal.state modal


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eModal, False <$ eRealDelete]

    bSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionUser
        , Nothing <$ eRealDelete
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionUser
        <*> bShowDataUser
        <@> eFilterUser
        ]

    bDatabaseLoan    <- asks Env.bDatabaseLoan
    bDatabaseUser    <- asks Env.bDatabaseUser
    bDatabaseItem    <- asks Env.bDatabaseItem
    bDatabaseToken   <- asks Env.bDatabaseToken
    bSelectionToken  <- asks Env.bSelectionToken
    bDatabaseHistory <- asks Env.bDatabaseHistory


    bLookupLoan      <- lookupLoan
    bLookupUser      <- lookupUser
    bSelectedToken   <- selectedToken


    let bShowDataUser :: Behavior (DatabaseKey -> String)
        bShowDataUser = (maybe "" User.name .) <$> bLookupUser

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowDataUser

        bLoanUser :: Behavior (DatabaseKey -> Maybe Int)
        bLoanUser = (fmap Loan.user .) <$> bLookupLoan

        bListBoxUsers :: Behavior [DatabaseKey]
        bListBoxUsers =
            (\p q show f ->
                    filter (f . Just)
                        . filter (flip List.notElem q)
                        . filter (p . show)
                        . keys
                )
                <$> bFilterUser
                <*> bUsersWithLoan
                <*> bShowDataUser
                <*> isSelectedCurrentUser'
                <*> bDatabaseUser

        bSelectionDataUser :: Behavior (Maybe User)
        bSelectionDataUser = (=<<) <$> bLookupUser <*> bSelectionUser


        bUsersWithLoan :: Behavior [DatabaseKey]
        bUsersWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanUser <*> bDatabaseLoan


    let bHasSelectedUser :: Behavior Bool
        bHasSelectedUser =
            (\x xs -> case x of
                    Nothing -> False
                    Just y  -> List.elem y xs
                )
                <$> bSelectionUser
                <*> bListBoxUsers


        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken

        isSelectedCurrentUser' :: Behavior (Maybe Int -> Bool)
        isSelectedCurrentUser' = (/=) <$> bSelectedTokenId

        isSelectedCurrentUser :: Behavior Bool
        isSelectedCurrentUser = (/=) <$> bSelectedTokenId <*> bSelectionUser

    liftUI $ element deleteBtn # sink
        UI.enabled
        (bHasSelectedUser <&&> isSelectedCurrentUser)


    return (elem, filterJust $ bSelectionUser <@ eRealDelete)
