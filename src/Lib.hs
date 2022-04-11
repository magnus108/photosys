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

import qualified MenuBox

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

    --dangerMove
    (loginGui, currentUser, (loginBtn, logoutBtn), bLogin, bUser) <- LoginGui.setup window



    let datastore = "data/tab.json"
    database <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI
            (Database Tab)


    listBox <- MenuBox.listBox currentUser bTabPairsFilter bSelection bDisplayDataItem

    menu <-
        UI.mkElement "nav"
        #. "navbar is-primary is-spaced"
        #+ [ UI.div
             #. "container"
             #+ [ element listBox
                , UI.div
                #. "navbar-menu"
                #+ [UI.div #. "navbar-start", UI.div #. "navbar-end" #+ [UI.div #. "navbar-item" #+ [element logoutBtn]]]
                ]
           ]

    tab     <- dataItem bSelectionDataItemFilter menu loginGui (loginBtn,logoutBtn) bLogin bUser




    getBody window #+ [element tab]


    let eSelection = rumors $ MenuBox.userSelection listBox

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


    let isAdmin = maybe False User.admin <$> bUser

        bTabPairs :: Behavior [(DatabaseKey, DataItem)]
        bTabPairs = toPairs <$> bDatabase

        bTabPairsFilter :: Behavior [DatabaseKey]
        bTabPairsFilter = fmap fst <$> ((\admin xs -> filter (\x -> admin == Tab.admin (snd x)) xs) <$> isAdmin <*> bTabPairs)

        bTabItemsFilter :: Behavior [Maybe DataItem]
        bTabItemsFilter = fmap <$> bLookup <*> bTabPairsFilter

        bSelectionDataItemFilter :: Behavior (Maybe DataItem)
        bSelectionDataItemFilter = (\x xs -> if elem x xs then x else Nothing) <$> bSelectionDataItem <*> bTabItemsFilter



    return ()


type DataItem = Tab

showDataItem t = Tab.name t

emptyDataItem = Tab ""

dataItem :: Behavior (Maybe DataItem) -> Element -> Element -> (Element, Element) -> Behavior Bool -> Behavior (Maybe User) -> UI Element
dataItem bItem tabs loginGui (loginBtn,logoutBtn) bLogin bUser = mdo
    window  <- askWindow


    (loanGui, bDatabaseLoan) <- LoanGui.setup window
    -------------------------------------------------------------------------------------
    let datastoreUser = "data/user.json"
    databaseUser <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreUser :: UI
            (Database User)

    (userGui, eCreate, bSelectionCreate, eDataItemIn) <- UserGui.setup window bDatabaseUser bUser
    (deleteUserGui, eDelete, bSelectionDelete, bUsersWithLoan) <- DeleteUserGui.setup window bDatabaseUser bUser bDatabaseLoan  -- BTOKEN SKAL INDEHOLDE EN USERJO!

    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions
        [ create (User "" "" False) <$ eCreate
        , filterJust $ update' <$> bSelectionCreate <@> eDataItemIn
        , delete <$> filterJust (bSelectionDelete <@ eDelete)
        ]


    onChanges bDatabaseUser $ \items -> do
        liftIO $ BS.writeFile datastoreUser $ toStrict $ encode items
    -------------------------------------------------------------------------------------

    let datastoreItem = "data/item.json"
    databaseItem <- liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreItem :: UI (Database Item)

    (itemGui, eCreateItem, bSelectionCreateItem, eDataItemInItem)<- ItemGui.setup window bDatabaseItem


    bDatabaseItem <- accumB databaseItem $ concatenate <$> unions
        [ create (Item "" "") <$ eCreateItem
        , filterJust $ update' <$> bSelectionCreateItem <@> eDataItemInItem
    --    , delete <$> filterJust (bSelection <@ eDelete)
        ]

    onChanges bDatabaseItem $ \items -> do
        liftIO $ BS.writeFile datastoreItem $ toStrict $ encode items

    ---------------------------------------------------------------------------




    deleteItemGui <- DeleteItemGui.setup window
    handInGui <- HandInGui.setup window



    let display y x = if y
            then case Tab.name x of
                "Create Item"    -> [tabs, itemGui]
                "Delete Item"    -> [tabs, deleteItemGui]
                "Hand in" -> [tabs, handInGui]
                "Loan" -> [tabs, loanGui]
                "Create User"    -> [tabs, userGui]
                "Delete User"    -> [tabs, deleteUserGui]
            else [loginGui]



    let bGui = display <$> bLogin

    content <- UI.div # sink children (maybe [loginGui] <$> bGui <*> bItem)

    element content
