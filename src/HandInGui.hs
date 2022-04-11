{-# LANGUAGE RecursiveDo #-}
module HandInGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Loan
import           Item                           ( Item )
import qualified Item
import           User                           ( User )
import qualified User

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString               as BS

import           Database


setup
    :: Window
    -> Behavior (Database DataItem)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> UI (Element, Event (), Behavior (Maybe DatabaseKey))
setup window bDatabase bDatabaseUser bDatabaseItem = mdo

    -- GUI elements
    deleteBtn   <- UI.button #+ [string "Aflever"]
    listBox     <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry <- UI.entry bFilterString

    -- GUI layout
    search      <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [ element filterEntry #. "input" # set
                    (attr "placeholder")
                    "Fx Anders Andersen eller Kamera 1"
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

    let bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

    let bLookupItem :: Behavior (DatabaseKey -> Maybe Item)
        bLookupItem = flip lookup <$> bDatabaseItem


        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" showDataItem .) <$> bLookup

        bShowDataItem2 :: Behavior (DatabaseKey -> String)
        bShowDataItem2 = (\l u i k -> case l k of
                                        Nothing -> ""
                                        Just (Loan item user) ->
                                            case u user of
                                              Nothing -> ""
                                              Just user' ->
                                                  case i item of
                                                    Nothing -> ""
                                                    Just item' ->
                                                        User.name user' ++ ", " ++ (Item.name item')
                           ) <$> bLookup <*> bLookupUser <*> bLookupItem

        bDisplayDataItem = (UI.string .) <$> bShowDataItem2

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilter
                <*> bShowDataItem
                <*> bDatabase


    let bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bSelection

    element deleteBtn # sink UI.enabled bDisplayItem

    return (elem, eDelete, bSelection)

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = Loan

showDataItem :: DataItem -> String
showDataItem i = show (item i) ++ ", " ++ (show (user i))
