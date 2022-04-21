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


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event DatabaseKey)
setup window = mdo
    -- GUI elements
    filterUser  <- liftUI $ UI.entry bFilterEntryUser
    listBoxUser <- liftUI $ UI.listBox bListBoxUsers bSelectionUser bDisplayUserName
    counter <- liftUI $ Counter.counter bListBoxUsers

    deleteBtn   <- liftUI $ UI.button #+ [string "Slet"]

    -- GUI layout
    searchUser  <- liftUI $
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
            , UI.div
            #. "control"
            #+ [ element filterUser #. "input" # set (attr "placeholder")
                                                    "Fx Anders Andersen"
                ]
            ]

    dropdownUser <- liftUI $
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
                #+ [UI.div #. "box" #+ [string "Sletning godkendt"]]
                , element closeBtn
                ]

    elem <- liftUI $
        UI.div
        #. "section is-medium"
        #+ [ UI.div
                #. "container"
                #+ [ element searchUser
                , element dropdownUser
                , element deleteBtn'
                , element counter
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

    bDatabaseLoan                      <- asks Env.bDatabaseLoan
    bDatabaseUser                      <- asks Env.bDatabaseUser
    bDatabaseItem                      <- asks Env.bDatabaseItem
    bDatabaseToken                     <- asks Env.bDatabaseToken
    bSelectionToken                    <- asks Env.bSelectionToken
    bDatabaseHistory                   <- asks Env.bDatabaseHistory



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

    liftUI $ element deleteBtn
        # sink UI.enabled (bHasSelectedUser <&&> isSelectedCurrentUser)

    liftUI $ element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    return (elem, filterJust $ bSelectionUser <@ eDelete)
