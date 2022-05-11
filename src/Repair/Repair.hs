{-# LANGUAGE RecursiveDo #-}
module Repair.Repair where

import           Utils.Utils
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

import           Repair                         ( Repair )
import qualified Repair

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
    (filterRepair, searchRepair  ) <- mkSearch bFilterEntryRepair
    (listBoxRepair, dropdownRepair) <- mkListBox bListBoxRepairs
                                             bSelectionRepair
                                             bDisplayRepair
    counterRepair                 <- mkCounter bListBoxRepairs

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


    (deleteBtn, deleteBtnView) <- mkButton "Tilbagelever"

    -- GUI layout
    closeBtn                   <- liftUI $ UI.button #. "modal-close is-large"
    modal                      <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-content"
           #+ [ UI.div
                #. "box"
                #+ [string "Tilbagelever godkendt"]
              ]
           , element closeBtn
           ]

    elem <- mkContainer
        [ element searchUser
        , element dropdownUser
        , element counterUser
        , element searchItem
        , element dropdownItem
        , element counterItem
        , element searchRepair
        , element dropdownRepair
        , element deleteBtnView
        , element counterRepair
        , element modal
        ]


    -- Events and behaviors
    bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem
    bFilterEntryRepair <- stepper "" . rumors $ UI.userText filterRepair



    let tFilterRepair = isInfixOf <$> UI.userText filterRepair
        bFilterRepair = facts tFilterRepair
        eFilterRepair = rumors tFilterRepair

    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem
        eSelectionRepair = rumors $ UI.userSelection listBoxRepair
        eDelete        = UI.click deleteBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose]

    bSelectionRepair <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionRepair
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionRepair
        <*> bShowRepair
        <@> eFilterRepair
        ]


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
        , Nothing <$ eDelete
        ]

    bLastLoanItem <- stepper Nothing $ Unsafe.head <$> unions
        [bSelectionItem <@ eDelete]

    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseRepair <- asks Env.bDatabaseRepair
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken


    bLookupRepair <- lookupRepair
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

        bShowRepair :: Behavior (DatabaseKey -> String)
        bShowRepair = (maybe "" (show . Repair.loan) .) <$> bLookupRepair

        bDisplayRepair :: Behavior (DatabaseKey -> UI Element)
        bDisplayRepair = (UI.string .) <$> bShowRepair

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.showItem .) <$> bLookupItem

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem


        bLastLoanItemItem :: Behavior (Maybe Item)
        bLastLoanItemItem = (=<<) <$> bLookupItem <*> bLastLoanItem




        bRepairs :: Behavior [Repair]
        bRepairs = (\db lookupRepair -> catMaybes $ fmap lookupRepair (keys db)) <$> bDatabaseRepair <*> bLookupRepair

        bLoans :: Behavior [(Maybe Loan, Repair)]
        bLoans = (\lookup repairs -> fmap (\x -> (lookup (Repair.loan x), x)) repairs) <$> bLookupLoan <*> bRepairs

        bLoans2 :: Behavior [(User, Item, Repair)]
        bLoans2 = (\lookupUser lookupItem lookupRepair -> catMaybes . fmap (\(ml, r) -> liftA3 (,,) (lookupUser =<< (Loan.user <$> ml)) (lookupItem =<< (Loan.item <$> ml)) (Just r))) <$> bLookupUser <*> bLookupItem <*> bLookupRepair <*> bLoans

        bLoans3 :: Behavior [(User, Item, Repair)]
        bLoans3 = (\i -> filter (\x -> Just (snd3 x) == i || i == Nothing )) <$> bSelectedItem <*> bLoans2

        bLoans4 :: Behavior [(User, Item, Repair)]
        bLoans4 = (\i -> filter (\x -> Just (fst3 x) == i || i == Nothing )) <$> bSelectedUser <*> bLoans3

        bLoans5 :: Behavior [(User, Item, Repair)]
        bLoans5 = (\i -> filter (\x -> Just (thd3 x) == i || i == Nothing )) <$> bSelectedRepair <*> bLoans4


        bListBoxRepairs :: Behavior [DatabaseKey]
        bListBoxRepairs = (\p show lookup pairs -> filter (flip List.elem (fmap (Just . thd3) pairs) . lookup) . filter (p. show) . keys)
                        <$> bFilterRepair <*> bShowRepair <*> bLookupRepair <*> bLoans5 <*> bDatabaseRepair

        bListBoxUsers :: Behavior [DatabaseKey]
        bListBoxUsers = (\p show lookup pairs -> filter (flip List.elem (fmap (Just . fst3) pairs) . lookup) . filter (p. show) . keys)
                        <$> bFilterUser <*> bShowUser <*> bLookupUser <*> bLoans5 <*> bDatabaseUser

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems  = (\p show lookup pairs -> filter (flip List.elem (fmap (Just . snd3) pairs) . lookup) . filter (p. show) . keys)
                        <$> bFilterItem <*> bShowItem <*> bLookupItem <*> bLoans5 <*> bDatabaseItem

        bSelectedRepair :: Behavior (Maybe Repair)
        bSelectedRepair = (=<<) <$> bLookupRepair <*> bSelectionRepair

        hasSelectedRepair :: Behavior Bool
        hasSelectedRepair = isJust <$> bSelectedRepair


    liftUI $ element deleteBtn # sink UI.enabled hasSelectedRepair
    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    return (elem, filterJust $ bSelectionRepair <@ eDelete)


fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c
