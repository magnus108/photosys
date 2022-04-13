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


setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> Behavior (Database Token)
    -> Behavior (Maybe DatabaseKey)
    -> UI (Element, Event DatabaseKey)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bSelectionToken
    = mdo

    -- GUI elements
        filterUser  <- UI.entry bFilterEntryUser
        listBoxUser <- UI.listBox bListBoxUsers bSelectionUser bDisplayUserName

        deleteBtn   <- UI.button #+ [string "Slet"]

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


        deleteBtn' <-
            UI.div
            #. "field"
            #+ [UI.div #. "control" #+ [element deleteBtn #. "button"]]


        closeBtn <- UI.button #. "modal-close is-large"

        modal    <-
            UI.div
                #+ [ UI.div #. "modal-background"
                   , UI.div
                   #. "modal-content"
                   #+ [UI.div #. "box" #+ [string "Sletning godkendt"]]
                   , element closeBtn
                   ]

        elem <-
            UI.div
            #. "section is-medium"
            #+ [ UI.div
                 #. "container"
                 #+ [ element searchUser
                    , element dropdownUser
                    , element deleteBtn'
                    , element modal
                    ]
               ]


        -- Events and behaviors
        bFilterEntryUser <- stepper "" . rumors $ UI.userText filterUser


        let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
            isInfixOf needle haystack =
                any (isPrefixOf needle) (tails haystack)

        let tFilterUser = isInfixOf <$> UI.userText filterUser
            bFilterUser = facts tFilterUser
            eFilterUser = rumors tFilterUser

        let eSelectionUser = rumors $ UI.userSelection listBoxUser
            eDelete        = UI.click deleteBtn
            eClose         = UI.click closeBtn


        bActiveModal <- stepper False $ Unsafe.head <$> unions
            [True <$ eDelete, False <$ eClose]

        bSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
            [ eSelectionUser
            , Nothing <$ eDelete
            , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
            <$> bSelectionUser
            <*> bShowDataUser
            <@> eFilterUser
            ]


        let bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
            bLookupLoan = flip lookup <$> bDatabaseLoan

            bLookupUser :: Behavior (DatabaseKey -> Maybe User)
            bLookupUser = flip lookup <$> bDatabaseUser

            bShowDataUser :: Behavior (DatabaseKey -> String)
            bShowDataUser = (maybe "" User.name .) <$> bLookupUser

            bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
            bDisplayUserName = (UI.string .) <$> bShowDataUser

            bLoanUser :: Behavior (DatabaseKey -> Maybe Int)
            bLoanUser = (fmap Loan.user .) <$> bLookupLoan

            bListBoxUsers :: Behavior [DatabaseKey]
            bListBoxUsers =
                (\p q show f ->
                        filter (f . Just)  . filter (flip List.notElem q) . filter (p . show) . keys
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
                (\f -> catMaybes . fmap f . keys)
                    <$> bLoanUser
                    <*> bDatabaseLoan


        let bHasSelectedUser :: Behavior Bool
            bHasSelectedUser =
                (\x xs -> case x of
                        Nothing -> False
                        Just y  -> List.elem y xs
                    )
                    <$> bSelectionUser
                    <*> bListBoxUsers

            bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
            bLookupToken = flip lookup <$> bDatabaseToken

            bSelectedToken :: Behavior (Maybe Token)
            bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken

            bSelectedTokenId :: Behavior (Maybe Int)
            bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken

            isSelectedCurrentUser' :: Behavior (Maybe Int -> Bool)
            isSelectedCurrentUser' =
                (/=) <$> bSelectedTokenId

            isSelectedCurrentUser :: Behavior Bool
            isSelectedCurrentUser =
                (/=) <$> bSelectedTokenId <*> bSelectionUser

        element deleteBtn
            # sink UI.enabled (bHasSelectedUser <&&> isSelectedCurrentUser)
        element modal # sink
            (attr "class")
            ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

        return (elem, filterJust $ bSelectionUser <@ eDelete)
