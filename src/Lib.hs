{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe
import qualified Relude.Container.Reexport     as Reexport
import qualified Relude.Extra.Map              as Map

someFunc :: Int -> IO ()
someFunc port = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just "static"
                           , jsCustomHTML               = Just "index.html"
                           }
        $ setup


setup :: Window -> UI ()
setup window = void $ mdo
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


    -- database
    -- bDatabase :: Behavior (Database DataItem)
    let update' mkey x = flip update x <$> mkey
    bDatabase <- accumB emptydb $ concatenate <$> unions
        [ create (Item "CAM" "0123dd12") <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        , delete <$> filterJust (bSelection <@ eDelete)
        ]

    -- selection
    -- bSelection :: Behavior (Maybe DatabaseKey)
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

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

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
        liftIO $ putStrLn (show items)


{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Reexport.Map DatabaseKey a }
        deriving (Show)


emptydb = Database 0 mempty
keys = Map.keys . db

create x (Database newkey db) = Database (newkey + 1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey $ Map.insert key x db
delete key (Database newkey db) = Database newkey $ Map.delete key db
lookup key (Database _ db) = Map.lookup key db


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
