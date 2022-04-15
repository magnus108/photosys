{-# LANGUAGE RecursiveDo #-}
module Loan.Create where

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

liftA4
    :: Applicative f
    => (a -> b -> c -> d -> e)
    -> f a
    -> f b
    -> f c
    -> f d
    -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> Behavior (Database Token)
    -> Behavior (Maybe DatabaseKey)
    -> UI (Element, Event Loan)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bSelectionToken = mdo

    -- GUI elements
    filterUser  <- UI.entry bFilterEntryUser
    listBoxUser <- UI.listBox bListBoxUsers bSelectionUser bDisplayUserName

    filterItem  <- UI.entry bFilterEntryItem
    listBoxItem <- UI.listBox bListBoxItems bSelectionItem bDisplayItemName

    createBtn   <- UI.button #+ [string "Lån"]

    -- GUI layout
    searchUser  <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterUser #. "input" # set (attr "placeholder")
                                                    "Fx Anders Andersen"
              ]
           ]

    dropdownUser <-
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBoxUser # set (attr "size") "5" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    searchItem <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterItem #. "input" # set (attr "placeholder")
                                                    "Fx Kamera"
              ]
           ]

    dropdownItem <-
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


    createBtn' <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]


    closeBtn <- UI.button #. "modal-close is-large"
    modal    <-
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Lån godkendt"]]
               , element closeBtn
               ]

    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element searchUser
                , element dropdownUser
                , element searchItem
                , element dropdownItem
                , element createBtn'
                , element modal
                ]
           ]


    -- Events and behaviors
    bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser
    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem


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
        eCreate        = UI.click createBtn
        eClose         = UI.click closeBtn


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]


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
        , Nothing <$ eCreate
        ]


    let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip lookup <$> bDatabaseLoan

        bLoanItem :: Behavior (DatabaseKey -> Maybe Int)
        bLoanItem = (fmap Loan.item .) <$> bLookupLoan

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

        bListBoxUsers :: Behavior [DatabaseKey]
        bListBoxUsers =
            (\p show -> filter (p . show) . keys)
                <$> bFilterUser
                <*> bShowUser
                <*> bDatabaseUser


        bItemsWithLoan :: Behavior [DatabaseKey]
        bItemsWithLoan =
            (\f -> catMaybes . fmap f . keys) <$> bLoanItem <*> bDatabaseLoan

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p q show ->
                    filter (flip List.notElem q) . filter (p . show) . keys
                )
                <$> bFilterItem
                <*> bItemsWithLoan
                <*> bShowItem
                <*> bDatabaseItem

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken


---------
    timer <- UI.timer # set UI.interval 1000
    let eTick = UI.tick timer

    (eTime, hTime) <- liftIO $ newEvent

    c              <- liftIO $ showGregorian . utctDay <$> getCurrentTime

    bTimer         <- stepper (Just c) $ Unsafe.head <$> unions [eTime]

    onEvent eTick $ \items -> do
        c <- liftIO $ showGregorian . utctDay <$> getCurrentTime
        liftIO $ hTime (Just c)

    UI.start timer
---------



    let bCreateLoan :: Behavior (Maybe Loan)
        bCreateLoan =
            liftA4 Loan.Loan
                <$> bSelectionItem
                <*> bSelectionUser
                <*> bSelectedTokenId
                <*> bTimer

        hasUserSelected :: Behavior Bool
        hasUserSelected = isJust <$> bSelectionUser

        hasItemSelected :: Behavior Bool
        hasItemSelected = isJust <$> bSelectionItem


    element createBtn # sink UI.enabled (hasUserSelected <&&> hasItemSelected)
    element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)


    return (elem, filterJust $ bCreateLoan <@ eCreate)
