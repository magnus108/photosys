{-# LANGUAGE RecursiveDo #-}
module Count.Count where

import           Utils.Utils
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
import           Count                          ( Count )
import qualified Count

import           Layout
import           Behaviors
import qualified Modal

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event DatabaseKey, Event DatabaseKey)
setup window = mdo
    -- GUI elements
    (filterItem , searchItem  ) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems'
                                             bSelectionItem
                                             bDisplayItemName
    counterItem                   <- mkCounter bListBoxItems'

    (filterCount , searchCount  ) <- mkSearch bFilterEntryCount
    (listBoxCount, dropdownCount) <- mkListBox bListBoxCounts
                                               bSelectionCount
                                               bDisplayCountName
    counterCount <- mkCounter bListBoxCounts

    (createBtn, createBtnView) <- mkButton "Optæl"
    (deleteBtn, deleteBtnView) <- mkButton "Fjern"

    -- GUI layout
    (modalView, modal) <- mkModal bActiveModal [UI.string "Optælling godkendt"]
    (modalView2, modal2) <- mkModal bActiveModal2 [UI.string "Fjern godkendt"]

    elem <- mkContainer
        [ element searchItem
        , element dropdownItem
        , element createBtnView
        , element counterItem
        , element searchCount
        , element dropdownCount
        , element deleteBtnView
        , element counterCount
        , element modalView
        , element modalView2
        ]

    -- Events and behaviors
    bFilterEntryItem  <- stepper "" . rumors $ UI.userText filterItem
    bFilterEntryCount <- stepper "" . rumors $ UI.userText filterCount


    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let tFilterCount    = isInfixOf <$> UI.userText filterCount
        bFilterCount    = facts tFilterCount
        eFilterCount    = rumors tFilterCount

        eSelectionItem  = rumors $ UI.userSelection listBoxItem
        eSelectionCount = rumors $ UI.userSelection listBoxCount

        eCreate         = UI.click createBtn
        eDelete         = UI.click deleteBtn

        eModal          = rumors $ Modal.state modal
        eModal2         = rumors $ Modal.state modal2

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, eModal]

    bActiveModal2 <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, eModal2]

    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , Nothing <$ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowItem
        <@> eFilterItem
        ]

    bSelectionCount <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionCount
        , Nothing <$ eDelete
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionCount
        <*> bShowCount
        <@> eFilterCount
        ]

    bDatabaseCount <- asks Env.bDatabaseCount
    bDatabaseItem  <- asks Env.bDatabaseItem

    bLookupItem    <- lookupItem
    bLookupCount   <- lookupCount

    let bSelectedItem :: Behavior (Maybe Item)
        bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.showItem .) <$> bLookupItem

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilterItem
                <*> bShowItem
                <*> bDatabaseItem

        bListBoxItems' :: Behavior [DatabaseKey]
        bListBoxItems' =
            (\xs ys -> filter (flip List.notElem xs) ys)
                <$> bListBoxCountsToItem
                <*> bListBoxItems
        bShowCount2 :: Behavior (DatabaseKey -> Maybe DatabaseKey)
        bShowCount2 = (fmap Count.item .) <$> bLookupCount


        bListBoxCountsToItem :: Behavior [DatabaseKey]
        bListBoxCountsToItem =
            (\p -> mapMaybe p . keys) <$> bShowCount2 <*> bDatabaseCount




    let bSelectedCount :: Behavior (Maybe Count)
        bSelectedCount = (=<<) <$> bLookupCount <*> bSelectionCount

        bShowCount :: Behavior (DatabaseKey -> String)
        bShowCount =
            (\f g ->
                    maybe "" (\x -> maybe "" Item.showItem (g (Count.item x))) . f
                )
                <$> bLookupCount
                <*> bLookupItem

        bDisplayCountName :: Behavior (DatabaseKey -> UI Element)
        bDisplayCountName = (UI.string .) <$> bShowCount

        bListBoxCounts :: Behavior [DatabaseKey]
        bListBoxCounts =
            (\p show -> filter (p . show) . keys)
                <$> bFilterCount
                <*> bShowCount
                <*> bDatabaseCount


    return
        ( elem
        , filterJust $ bSelectionItem <@ eCreate
        , filterJust $ bSelectionCount <@ eDelete
        )
