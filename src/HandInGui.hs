{-# LANGUAGE RecursiveDo #-}
module HandInGui where

import Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Loan

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString as BS

import Database


setup :: Window -> UI Element
setup window = mdo
    let datastore = "data/loan.json"
    database <- liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI (Database Loan)

    -- GUI elements
    deleteBtn                         <- UI.button #+ [string "Aflever"]
    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry                       <- UI.entry bFilterString


    -- GUI layout
    elem <- UI.div
             #. "container"
             #+ [ grid
                      [ [row [string "Søg", element filterEntry #. "input"]]
                      , [ UI.div
                        #. "select is-multiple"
                        #+ [ element listBox # set (attr "size") "8" # set
                                 (attr "multiple")
                                 ""
                           ]
                        ]
                      , [ row
                              [ element deleteBtn #. "button"
                              ]
                        ]
                      ]
                ]


    -- Events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry

    let
        isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilter = isInfixOf <$> UI.userText filterEntry
        bFilter = facts tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDelete     = UI.click deleteBtn


    bDatabase <- accumB database $ concatenate <$> unions
        [ delete <$> filterJust (bSelection <@ eDelete)
        ]

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
        bShowDataItem    = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show -> filter (p . show) . keys)
                <$> bFilter
                <*> bShowDataItem
                <*> bDatabase


    let bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bSelection

    element deleteBtn # sink UI.enabled bDisplayItem

    onChanges bDatabase $ \items -> do
        liftIO $ BS.writeFile datastore $ toStrict $ encode items


    return elem

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = Loan

showDataItem :: DataItem -> String
showDataItem i = item i ++ ", " ++ (show (user i))
