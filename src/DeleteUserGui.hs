{-# LANGUAGE RecursiveDo #-}
module DeleteUserGui where

import           Data.Aeson
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )
import qualified Data.List                     as List

import           User
import           Loan
import qualified Loan

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString               as BS

import           Database
import qualified Checkbox


setup
    :: Window
    -> Behavior (Database DataItem)
    -> Behavior (Maybe User)
    -> Behavior (Database Loan)
    -> UI
           ( Element
           , Event ()
           , Behavior (Maybe DatabaseKey)
           , Behavior Bool
           )
setup window bDatabase bUser bDatabaseLoan = mdo

    -- GUI elements
    deleteBtn   <- UI.button #+ [string "Delete"]
    listBox     <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry <- UI.entry bFilterString

    -- GUI layout
    search      <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterEntry #. "input" # set (attr "placeholder")
                                                     "Fx Anders Andersen"
              ]
           ]

    dropdown <-
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBox # set (attr "size") "8" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]

    button <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element deleteBtn #. "button"]]

    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [element search, element dropdown, element button]
           ]


    -- Events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry

    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilter = isInfixOf <$> UI.userText filterEntry
        bFilter = facts tFilter
        eFilter = rumors tFilter

    let eSelection = rumors $ UI.userSelection listBox
        eDelete    = UI.click deleteBtn


    bSelection <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelection
        , Nothing <$ eDelete
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelection
        <*> bShowDataItem
        <@> eFilter
        ]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" showDataItem .) <$> bLookup

        bShowDataItem2 :: Behavior (DatabaseKey -> String)
        bShowDataItem2   = (maybe "" name .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem2

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilter
                <*> bShowDataItem
                <*> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection


    let bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bSelection

        bIsUser :: Behavior Bool
        bIsUser        = (==) <$> bSelectionDataItem <*> bUser


        bLoans         = fmap Loan.user <$> elems <$> bDatabaseLoan
        bUsersWithLoan = fmap <$> bLookup <*> bLoans
        bHasLoans      = List.elem <$> bSelectionDataItem <*> bUsersWithLoan


    let enable =
            and <$> sequenceA [bDisplayItem, not <$> bIsUser, not <$> bHasLoans]

    element deleteBtn # sink UI.enabled enable


    return (elem, eDelete, bSelection, bIsUser)

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = User

showDataItem :: DataItem -> String
showDataItem item = name item
