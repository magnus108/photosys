{-# LANGUAGE RecursiveDo #-}
module History.History where

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


setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> Behavior (Database Token)
    -> Behavior (Maybe DatabaseKey)
    -> UI Element
setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bSelectionToken
    = mdo

    -- GUI elements
        filterUser  <- UI.entry bFilterEntryUser
        listBoxUser <- UI.listBox bListBoxUsers'' bSelectionUser bDisplayUserName

        filterItem  <- UI.entry bFilterEntryItem
        listBoxItem <- UI.listBox bListBoxItems'' bSelectionItem bDisplayItemName

        filterLoan  <- UI.entry bFilterEntryLoan
        listBoxLoan <- UI.listBox bListBoxLoans'' bSelectionLoan bDisplayLoanTime

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

        searchLoan <-
            UI.div
            #. "field"
            #+ [ UI.label #. "label" #+ [string "Søg"]
               , UI.div
               #. "control"
               #+ [ element filterLoan #. "input" # set (attr "placeholder")
                                                        "Dato"
                  ]
               ]

        dropdownLoan <-
            UI.div
            #. "field"
            #+ [ UI.div
                 #. "control is-expanded"
                 #+ [ UI.div
                      #. "select is-multiple is-fullwidth"
                      #+ [ element listBoxLoan # set (attr "size") "5" # set
                               (attr "multiple")
                               ""
                         ]
                    ]
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
                    , element searchLoan
                    , element dropdownLoan
                    ]
               ]


        -- Events and behaviors
        bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser
        bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem
        bFilterEntryLoan <- stepper "" . rumors $ UI.userText filterLoan


        let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
            isInfixOf needle haystack =
                any (isPrefixOf needle) (tails haystack)

        let tFilterUser = isInfixOf <$> UI.userText filterUser
            bFilterUser = facts tFilterUser
            eFilterUser = rumors tFilterUser

        let tFilterItem = isInfixOf <$> UI.userText filterItem
            bFilterItem = facts tFilterItem
            eFilterItem = rumors tFilterItem

        let tFilterLoan = isInfixOf <$> UI.userText filterLoan
            bFilterLoan = facts tFilterLoan
            eFilterLoan = rumors tFilterLoan

        let eSelectionUser = rumors $ UI.userSelection listBoxUser
            eSelectionItem = rumors $ UI.userSelection listBoxItem
            eSelectionLoan = rumors $ UI.userSelection listBoxLoan


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
            ]

        bSelectionLoan <- stepper Nothing $ Unsafe.head <$> unions
            [ eSelectionLoan
            , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelectionLoan
            <*> bShowLoan
            <@> eFilterLoan
            ]


        let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
            bLookupUser = flip lookup <$> bDatabaseUser

            bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
            bLookupLoan = flip lookup <$> bDatabaseLoan

            bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
            bLookupItem = flip lookup <$> bDatabaseItem

            bSelectedUser :: Behavior (Maybe User)
            bSelectedUser = (=<<) <$> bLookupUser <*> bSelectionUser

            bSelectedItem :: Behavior (Maybe Item)
            bSelectedItem = (=<<) <$> bLookupItem <*> bSelectionItem

            bSelectedLoan :: Behavior (Maybe Loan)
            bSelectedLoan = (=<<) <$> bLookupLoan <*> bSelectionLoan

            bShowUser :: Behavior (DatabaseKey -> String)
            bShowUser = (maybe "" User.name .) <$> bLookupUser

            bShowItem :: Behavior (DatabaseKey -> String)
            bShowItem = (maybe "" Item.name .) <$> bLookupItem

            bShowLoan :: Behavior (DatabaseKey -> String)
            bShowLoan = (maybe "" Loan.timestamp .) <$> bLookupLoan

            bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
            bDisplayUserName = (UI.string .) <$> bShowUser

            bDisplayLoanTime :: Behavior (DatabaseKey -> UI Element)
            bDisplayLoanTime = (UI.string .) <$> bShowLoan

            bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
            bDisplayItemName = (UI.string .) <$> bShowItem

            bListBoxLoans :: Behavior [DatabaseKey]
            bListBoxLoans =
                (\p show -> filter (p . show) . keys)
                    <$> bFilterLoan
                    <*> bShowLoan
                    <*> bDatabaseLoan

            bListBoxLoans' :: Behavior [DatabaseKey]
            bListBoxLoans' =
                (\mi lookup -> filter ((\ml -> fromMaybe True (liftA2 (\l i -> Loan.item l == i) ml mi)) . lookup))
                    <$> bSelectionItem
                    <*> bLookupLoan
                    <*> bListBoxLoans

            bListBoxLoans'' :: Behavior [DatabaseKey]
            bListBoxLoans'' =
                (\mu lookup -> filter ((\ml -> fromMaybe True (liftA2 (\l u -> Loan.user l == u) ml mu)) . lookup))
                    <$> bSelectionUser
                    <*> bLookupLoan
                    <*> bListBoxLoans'

            bListBoxUsers :: Behavior [DatabaseKey]
            bListBoxUsers =
                (\p show -> filter (p . show) . keys)
                    <$> bFilterUser
                    <*> bShowUser
                    <*> bDatabaseUser


            bListBoxUsers' :: Behavior [DatabaseKey]
            bListBoxUsers' =
                (\loan users -> filter
                        (\userKey -> maybe True (\l -> Loan.user l == userKey) loan)
                        users
                    )
                    <$> bSelectedLoan
                    <*> bListBoxUsers

            bListBoxUsers'' :: Behavior [DatabaseKey]
            bListBoxUsers'' =
                (\loans lookup -> filter (flip List.elem (catMaybes (fmap Loan.user <$> (lookup <$> loans)))))
                    <$> bListBoxLoans''
                    <*> bLookupLoan
                    <*> bListBoxUsers'


            bListBoxItems :: Behavior [DatabaseKey]
            bListBoxItems =
                (\p show -> filter (p . show) . keys)
                    <$> bFilterItem
                    <*> bShowItem
                    <*> bDatabaseItem

            bListBoxItems' :: Behavior [DatabaseKey]
            bListBoxItems' =
                (\loan items -> filter
                        (\itemKey -> maybe True (\l -> Loan.item l == itemKey) loan)
                        items
                    )
                    <$> bSelectedLoan
                    <*> bListBoxItems

            bListBoxItems'' :: Behavior [DatabaseKey]
            bListBoxItems'' =
                (\loans lookup -> filter (flip List.elem (catMaybes (fmap Loan.item <$> (lookup <$> loans)))))
                    <$> bListBoxLoans''
                    <*> bLookupLoan
                    <*> bListBoxItems'

        return elem
