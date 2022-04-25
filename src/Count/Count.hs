{-# LANGUAGE RecursiveDo #-}
module Count.Count where

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
import           Count                           ( Count )
import qualified Count


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event DatabaseKey, Event DatabaseKey)
setup window = mdo
    -- GUI elements
    filterItem  <- liftUI $ UI.entry bFilterEntryItem
    listBoxItem <- liftUI $ UI.listBox bListBoxItems' bSelectionItem bDisplayItemName
    counterItem <- liftUI $ Counter.counter bListBoxItems'
    
    filterCount <- liftUI $ UI.entry bFilterEntryCount
    listBoxCount <- liftUI $ UI.listBox bListBoxCounts bSelectionCount bDisplayCountName
    counterCount <- liftUI $ Counter.counter bListBoxCounts

    createBtn   <- liftUI $ UI.button #+ [string "Optæl"]
    deleteBtn   <- liftUI $ UI.button #+ [string "Fjern"]

    -- GUI layout
    searchItem <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterItem #. "input" # set (attr "placeholder")
                                                    "Fx Kamera"
              ]
           ]

    dropdownItem <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBoxItem # set (attr "size") "5" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    createBtn' <- liftUI $
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]

    searchCount <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterCount #. "input" # set (attr "placeholder")
                                                    "Fx Kamera"
              ]
           ]

    dropdownCount <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBoxCount # set (attr "size") "5" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    deleteBtn' <- liftUI $
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element deleteBtn #. "button"]]

    closeBtn <- liftUI $ UI.button #. "modal-close is-large"

    modal    <- liftUI $
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Optælling godkendt"]]
               , element closeBtn
               ]


    closeBtn2 <- liftUI $ UI.button #. "modal-close is-large"

    modal2    <- liftUI $
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Fjern godkendt"]]
               , element closeBtn2
               ]

    elem <-
        liftUI $ UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchItem
                , element dropdownItem
                , element createBtn'
                , element counterItem
                , element searchCount
                , element dropdownCount
                , element deleteBtn'
                , element counterCount
                , element modal
                , element modal2
                ]
           ]

    -- Events and behaviors
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem
    bFilterEntryCount <- stepper "" . rumors $ UI.userText filterCount

    
    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let tFilterCount = isInfixOf <$> UI.userText filterCount
        bFilterCount = facts tFilterCount
        eFilterCount = rumors tFilterCount

        eSelectionItem = rumors $ UI.userSelection listBoxItem
        eSelectionCount = rumors $ UI.userSelection listBoxCount

        eClose         = UI.click closeBtn
        eClose2         = UI.click closeBtn2
        eCreate        = UI.click createBtn
        eDelete        = UI.click deleteBtn

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]

    bActiveModal2 <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose2]

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
    bDatabaseItem   <- asks Env.bDatabaseItem

    let bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

        bSelectedItem :: Behavior (Maybe Item)
        bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

        bShowItem :: Behavior (DatabaseKey -> String)
        bShowItem = (maybe "" Item.showItem .) <$> bLookupItem

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilterItem <*> bShowItem <*> bDatabaseItem

        bListBoxItems' :: Behavior [DatabaseKey]
        bListBoxItems' =
            (\xs ys -> filter (flip List.notElem xs) ys)
                <$> bListBoxCountsToItem
                <*> bListBoxItems
        bShowCount2 :: Behavior (DatabaseKey -> Maybe DatabaseKey)
        bShowCount2 = (fmap Count.item .) <$> bLookupCount


        bListBoxCountsToItem :: Behavior [DatabaseKey]
        bListBoxCountsToItem = (\p -> mapMaybe p . keys) <$> bShowCount2 <*> bDatabaseCount



    let bLookupCount :: Behavior (DatabaseKey -> Maybe Count)
        bLookupCount = flip lookup <$> bDatabaseCount

        bSelectedCount :: Behavior (Maybe Count)
        bSelectedCount = (=<<) <$> bLookupCount <*> bSelectionCount

        bShowCount :: Behavior (DatabaseKey -> String)
        bShowCount = (\f g -> maybe "" (\x -> maybe "" Item.showItem (g (Count.item x))) . f) <$> bLookupCount <*> bLookupItem

        bDisplayCountName :: Behavior (DatabaseKey -> UI Element)
        bDisplayCountName = (UI.string .) <$> bShowCount

        bListBoxCounts :: Behavior [DatabaseKey]
        bListBoxCounts = (\p show -> filter (p. show) . keys)
                    <$> bFilterCount <*> bShowCount <*> bDatabaseCount


    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    liftUI $ element modal2 # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal2)

    return (elem, filterJust $ bSelectionItem <@ eCreate, filterJust $ bSelectionCount <@ eDelete)
