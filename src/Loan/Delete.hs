{-# LANGUAGE RecursiveDo #-}
module Loan.Delete where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

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
    counterUser <- liftUI $ Counter.counter bListBoxUsers

    (filterItem , searchItem  ) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems
                                             bSelectionItem
                                             bDisplayItemName
    counterItem <- liftUI $ Counter.counter bListBoxItems


    loanInfo    <- liftUI $ UI.span
    (deleteBtn, deleteBtnView) <- mkButton "Aflever"

    -- GUI layout
    closeBtn                   <- liftUI $ UI.input # set UI.type_ "button" #. "button" # set value "Luk"
    modal                      <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-card"
           #+ [ UI.mkElement "section"
              #. "modal-card-body"
              #+ [string "Aflevering godkendt: ", element loanInfo]
              , UI.mkElement "footer" #. "modal-card-foot" #+ [element closeBtn]
              ]
           ]

    elem <- mkContainer
             [ element searchUser
                , element dropdownUser
                , element counterUser
                , element searchItem
                , element dropdownItem
                , element deleteBtnView
                , element counterItem
                , element modal
           ]


    -- Events and behaviors
    let eDelete        = UI.click deleteBtn
    bFilterEntryUser <- stepper "" $ Unsafe.head <$> unions [rumors $ UI.userText filterUser, "" <$ eDelete]
    bFilterEntryItem <- stepper "" $ Unsafe.head <$> unions [rumors $ UI.userText filterItem, "" <$ eDelete]


    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem
        eClose         = UI.click closeBtn
        ePress = UI.keypress closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose, False <$ ePress]


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


    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s items p -> case filter (p . s) (keys items) of
                                (x:[]) -> Just x
                                (xs) -> b >>= \a -> if p (s a) then Just a else Nothing
          )
        <$> bSelectionItem
        <*> bShowItem
        <*> bDatabaseItem
        <@> eFilterItem
        , Nothing <$ eDelete
        ]

    bLastLoanItem <- stepper Nothing $ Unsafe.head <$> unions
        [bSelectionItem <@ eDelete]

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

        bLoanUser :: Behavior (DatabaseKey -> Maybe Int)
        bLoanUser = (fmap Loan.user .) <$> bLookupLoan

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

        hasSelectedLoan :: Behavior Bool
        hasSelectedLoan = isJust <$> bSelectedLoan

        bLastLoanItemItem :: Behavior (Maybe Item)
        bLastLoanItemItem = (=<<) <$> bLookupItem <*> bLastLoanItem

    liftUI $ element loanInfo # sink
        text
        ((maybe "" Item.name) <$> bLastLoanItemItem)
    liftUI $ element deleteBtn # sink UI.enabled hasSelectedLoan
    liftUI $ element modal # sink (modalSink closeBtn) bActiveModal

    return (elem, filterJust $ bSelectedLoan <@ eDelete)

modalSink e = mkWriteAttr $ \b x -> void $ do
                return x # set (attr "class") (if b then "modal is-active" else "modal")
                if b then (UI.setFocus e) else return ()
