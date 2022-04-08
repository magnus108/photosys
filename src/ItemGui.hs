{-# LANGUAGE RecursiveDo #-}
module ItemGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString.Lazy          as BS

import           Database

setup :: Window -> UI ()
setup window = void $ mdo
    let datastore = "data/item.json"
    database <- liftIO $ Unsafe.fromJust . decode <$> BS.readFile datastore :: UI (Database Item)


    return window # set title "PhotoApp"

    -- GUI elements
    createBtn                         <- UI.button #+ [string "Create"]
    deleteBtn                         <- UI.button #+ [string "Delete"]
    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry                       <- UI.entry bFilterString

    ((elemName, elemCode), tDataItem) <- dataItem bSelectionDataItem


    -- GUI layout
    let uiDataItems = grid
            [ [ string "Name:"
              , element elemName #. "input"
              , string "Code:"
              , element elemCode #. "input"
              ]
            ]

    getBody window
        #+ [ UI.div
             #. "container"
             #+ [ grid
                      [ [row [element filterEntry #. "input"]]
                      , [ UI.div
                        #. "select is-multiple"
                        #+ [ element listBox # set (attr "size") "8" # set
                                 (attr "multiple")
                                 ""
                           ]
                        , uiDataItems
                        ]
                      , [ row
                              [ element createBtn #. "button"
                              , element deleteBtn #. "button"
                              ]
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
        eDataItemIn = rumors $ tDataItem
        eCreate     = UI.click createBtn
        eDelete     = UI.click deleteBtn


    bDatabase <- accumB database $ concatenate <$> unions
        [ create (Item "" "") <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        , delete <$> filterJust (bSelection <@ eDelete)
        ]

    bSelection <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelection
        , Nothing <$ eDelete
        , Just . nextKey <$> bDatabase <@ eCreate
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelection
        <*> bShowDataItem
        <@> eFilter
        ]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" showDataItem .) <$> bLookup

        bShowDataItem2 :: Behavior (DatabaseKey -> String)
        bShowDataItem2    = (maybe "" name .) <$> bLookup

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

    element deleteBtn # sink UI.enabled bDisplayItem
    element elemName # sink UI.enabled bDisplayItem
    element elemCode # sink UI.enabled bDisplayItem

    onChanges bDatabase $ \items -> do
        liftIO $ BS.writeFile datastore (encode items)


{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = Item


showDataItem :: DataItem -> String
showDataItem item = name item ++ ", " ++ (code item)


emptyDataItem :: DataItem
emptyDataItem = Item "" ""


dataItem
    :: Behavior (Maybe DataItem) -> UI ((Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ name . fromMaybe emptyDataItem <$> bItem
    entry2 <- UI.entry $ code . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , Item <$> UI.userText entry1 <*> UI.userText entry2
        )
