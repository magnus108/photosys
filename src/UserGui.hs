{-# LANGUAGE RecursiveDo #-}
module UserGui where

import           Data.Aeson
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           User

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString as BS

import           Database
import qualified Checkbox


setup :: Window -> UI Element
setup window = mdo
    let datastore = "data/user.json"
    database <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI
            (Database User)


    -- GUI elements
    createBtn   <- UI.button #+ [string "Create"]
    listBox     <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry <- UI.entry bFilterString

    ((elemName, elemPassword, elemAdmin), tDataItem) <- dataItem bSelectionDataItem


    -- GUI layout
    let uiDataItems = grid
            [ [ string "Name:"
              , element elemName #. "input"
              , string "Password:"
              , element elemPassword #. "input" # set UI.type_ "password"
              , string "Admin:"
              , element elemAdmin #. "checkbox"
              ]
            ]

    elem <- UI.div
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
                              ]
                        ]
                      ]
                ]


    -- Events and behaviors
    bFilterString <- stepper "" . rumors $ UI.userText filterEntry

    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilter = isInfixOf <$> UI.userText filterEntry
        bFilter = facts tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDataItemIn = rumors $ tDataItem
        eCreate     = UI.click createBtn


    -- database
    -- bDatabase :: Behavior (Database DataItem)
    bDatabase <- accumB database $ concatenate <$> unions
        [ create (User "" "" False) <$ eCreate
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        ]

    -- selection
    -- bSelection :: Behavior (Maybe DatabaseKey)
    bSelection <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelection
        , Just . nextKey <$> bDatabase <@ eCreate
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

    element elemName # sink UI.enabled bDisplayItem
    element elemPassword # sink UI.enabled bDisplayItem
    element elemAdmin # sink UI.enabled bDisplayItem

    onChanges bDatabase $ \items -> do
        liftIO $ BS.writeFile datastore $ toStrict $ encode items

    return elem



{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = User

showDataItem :: DataItem -> String
showDataItem item = name item

emptyDataItem :: DataItem
emptyDataItem = User "" "" False

dataItem
    :: Behavior (Maybe DataItem)
    -> UI ((Element, Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ name . fromMaybe emptyDataItem <$> bItem
    entry2 <- UI.entry $ password . fromMaybe emptyDataItem <$> bItem
    entry3 <- Checkbox.entry $ admin . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2, getElement entry3)
        , User
        <$> UI.userText entry1
        <*> UI.userText entry2
        <*> Checkbox.userCheck entry3
        )
