{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe
import           Database


import User (User(..))
import qualified UserGui
import qualified DeleteUserGui
import qualified LoanGui
import qualified HandInGui
import qualified ItemGui
import qualified DeleteItemGui
import qualified LoginGui

import           Tab                            ( Tab(..) )
import qualified Tab

import qualified Data.ByteString               as BS


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

    let datastore = "data/tab.json"
    database <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI
            (Database Tab)

    listBox <- UI.listBox bListBoxItems bSelection bDisplayDataItem

    menu    <- element listBox
    tab     <- dataItem bSelectionDataItem menu

    getBody window #+ [element tab]


    let eSelection = rumors $ UI.userSelection listBox

    bDatabase  <- accumB database $ concatenate <$> unions []
    bSelection <- stepper (Just 5) $ Unsafe.head <$> unions [eSelection]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" showDataItem .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = keys <$> bDatabase

        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

    return ()


type DataItem = Tab

showDataItem t = Tab.name t

emptyDataItem = Tab ""

dataItem :: Behavior (Maybe DataItem) -> Element -> UI Element
dataItem bItem tabs = mdo
    window  <- askWindow


    -------------------------------------------------------------------------------------
    let datastoreUser = "data/user.json"
    databaseUser <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreUser :: UI
            (Database User)

    (userGui, eCreate, bSelectionCreate, eDataItemIn) <- UserGui.setup window bDatabaseUser
    (deleteUserGui, eDelete, bSelectionDelete) <- DeleteUserGui.setup window bDatabaseUser bUser

    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions
        [ create (User "" "" False) <$ eCreate
        , filterJust $ update' <$> bSelectionCreate <@> eDataItemIn
        , delete <$> filterJust (bSelectionDelete <@ eDelete)
        ]


    onChanges bDatabaseUser $ \items -> do
        liftIO $ BS.writeFile datastoreUser $ toStrict $ encode items
    -------------------------------------------------------------------------------------


    itemGui <- ItemGui.setup window
    deleteItemGui <- DeleteItemGui.setup window
    handInGui <- HandInGui.setup window
    loanGui <- LoanGui.setup window


    (loginGui, (loginBtn, logoutBtn), bLogin, bUser) <- LoginGui.setup window
    empty   <- string "fejl"

    login   <-
        UI.div
        #. "container"
        #+ [ grid
                 [ [element loginGui]
                 , [row [element loginBtn, element logoutBtn]]
                 ]
           ]

    let display y x = if y
            then case Tab.name x of
                "Create Item"    -> [tabs, logoutBtn, itemGui]
                "Delete Item"    -> [tabs, logoutBtn, deleteItemGui]
                "Hand in" -> [tabs, logoutBtn, handInGui]
                "Loan" -> [tabs, logoutBtn, loanGui]
                "Create User"    -> [tabs, logoutBtn, userGui]
                "Delete User"    -> [tabs, logoutBtn, deleteUserGui]
            else [login]

    let bGui = display <$> bLogin

    content <- UI.div # sink children (maybe [empty] <$> bGui <*> bItem)

    element content
