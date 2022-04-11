{-# LANGUAGE RecursiveDo #-}
module LoanGui where

import Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Loan

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString as BS

import Database


setup :: Window -> UI (Element, Behavior (Database Loan))
setup window = mdo
    let datastore = "data/loan.json"
    database <- liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI (Database Loan)

    -- GUI elements
    createBtn                         <- UI.button #+ [string "Create"]
    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem
    filterEntry                       <- UI.entry bFilterString

    ((elemName, elemItem), tDataItem) <- dataItem bSelectionDataItem

    -- GUI layout
    let uiDataItems = grid
            [ [ string "Name:"
              , element elemName #. "input"
              , string "Item:"
              , element elemItem #. "input"
              ]
            ]

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

    let
        isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilter = isInfixOf <$> UI.userText filterEntry
        bFilter = facts tFilter
        eFilter = rumors tFilter

    let eSelection  = rumors $ UI.userSelection listBox
        eDataItemIn = filterJust $ rumors $ tDataItem
        eCreate     = UI.click createBtn


    bDatabase <- accumB database $ concatenate <$> unions
        [ create (Loan 0 0) <$ eCreate --  BØR VÆRE NUVÆRNEDE BRUGER. MEN HVAD MED HVILKEN GENSTAND??
        , filterJust $ update' <$> bSelection <@> eDataItemIn
        ]

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

    element elemName # sink UI.enabled bDisplayItem
    element elemItem # sink UI.enabled bDisplayItem

    onChanges bDatabase $ \items -> do
        liftIO $ BS.writeFile datastore $ toStrict $ encode items


    return (elem, bDatabase)

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

type DataItem = Loan

showDataItem :: DataItem -> String
showDataItem i = show (item i) ++ ", " ++ (show (user i))


dataItem
    :: Behavior (Maybe DataItem) -> UI ((Element, Element), Tidings (Maybe DataItem))
dataItem bItem = do
    entry1 <- UI.entry $ maybe "" (show . item) <$> bItem
    entry2 <- UI.entry $ maybe "" (show . user) <$> bItem

    let maybeParse1 = readMaybe <$> UI.userText entry2
    let maybeParse2 = readMaybe <$> UI.userText entry2
    return
        ( (getElement entry1, getElement entry2 )
        , liftA2 Loan <$> maybeParse1 <*> maybeParse2
        )
