{-# LANGUAGE RecursiveDo #-}
module Loan.DeleteNormal where

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


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event DatabaseKey)
setup window = mdo
    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken

    -- GUI elements
    filterItem  <- liftUI $ UI.entry bFilterEntryItem
    listBoxItem <- liftUI $ UI.listBox bListBoxItems bSelectionItem bDisplayItemName

    deleteBtn <- liftUI $ UI.button #+ [string "Aflever"]

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
               #+ [UI.div #. "box" #+ [string "Aflevering godkendt"]]
               , element closeBtn
               ]

    elem <- liftUI $ 
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchItem
                , element dropdownItem
                , element deleteBtn'
                , element modal
                ]
           ]


    -- Events and behaviors
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem


    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionItem = rumors $ UI.userSelection listBoxItem
        eDelete        = UI.click deleteBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose]


    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowItem
        <@> eFilterItem
        , Nothing <$ eDelete
        ]

    let bSelectionUser = bSelectedTokenId

    let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip lookup <$> bDatabaseLoan

        bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

        bLoanUser :: Behavior (DatabaseKey -> Maybe Int)
        bLoanUser = (fmap Loan.user .) <$> bLookupLoan

        bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem

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

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken

        hasSelectedLoan :: Behavior Bool
        hasSelectedLoan = isJust <$> bSelectedLoan

    liftUI $ element deleteBtn # sink UI.enabled hasSelectedLoan
    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    return (elem, filterJust $ bSelectedLoan <@ eDelete)

