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

import qualified Item.Create as ItemCreate
import qualified Item.Delete as ItemDelete

import qualified Loan.Create as LoanCreate
import qualified Loan.Delete as LoanDelete

import qualified Loan.CreateNormal as LoanCreateNormal
import qualified Loan.DeleteNormal as LoanDeleteNormal


import qualified User.Create as UserCreate
import qualified User.Delete as UserDelete

import qualified Token.Create as TokenCreate

import Loan (Loan(..))
import User (User(..))
import Token (Token(..))


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

    listBox <- MenuBox.listBox {-currentUser-} bListBoxItems {-bTabPairsFilter-} bSelection bDisplayDataItem

    menu <-
        UI.mkElement "nav"
        #. "navbar is-primary is-spaced"
        #+ [ UI.div
             #. "container"
             #+ [ element listBox
                , UI.div
                #. "navbar-menu"
                #+ [UI.div #. "navbar-start", UI.div #. "navbar-end" #+ [UI.div #. "navbar-item" #+ [{-element logoutBtn-}]]]
                ]
           ]

    tab     <- dataItem bSelectionDataItem {-bSelectionDataItemFilter-} menu


    getBody window #+ [element tab]


    let eSelection = rumors $ MenuBox.userSelection listBox

    bDatabase  <- accumB database $ concatenate <$> unions []
    bSelection <- stepper (Just 5) $ Unsafe.head <$> unions [eSelection]

    let bLookup :: Behavior (DatabaseKey -> Maybe DataItem)
        bLookup = flip lookup <$> bDatabase

        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem    = (maybe "" Tab.name .) <$> bLookup

        bDisplayDataItem = (UI.string .) <$> bShowDataItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems = keys <$> bDatabase


        bSelectionDataItem :: Behavior (Maybe DataItem)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection


    {-
    let isAdmin = maybe False User.admin <$> bUser

        bTabPairs :: Behavior [(DatabaseKey, DataItem)]
        bTabPairs = toPairs <$> bDatabase

        bTabPairsFilter :: Behavior [DatabaseKey]
        bTabPairsFilter = fmap fst <$> ((\admin xs -> filter (\x -> admin == Tab.admin (snd x)) xs) <$> isAdmin <*> bTabPairs)

        bTabItemsFilter :: Behavior [Maybe DataItem]
        bTabItemsFilter = fmap <$> bLookup <*> bTabPairsFilter

        bSelectionDataItemFilter :: Behavior (Maybe DataItem)
        bSelectionDataItemFilter = (\x xs -> if elem x xs then x else Nothing) <$> bSelectionDataItem <*> bTabItemsFilter
        -}


    return ()


type DataItem = Tab


dataItem :: Behavior (Maybe DataItem) -> Element -> UI Element
dataItem bItem tabs = mdo
    window  <- askWindow

    ----------------------------------------------------------------------------------

    let datastoreToken = "data/token.json"
    databaseToken <- liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreToken :: UI (Database Token)

    (tokenCreate, eTokenCreate) <- TokenCreate.setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bTokenSelection

    bTokenSelection <- stepper (Just 0) UI.never
    bDatabaseToken <- accumB databaseToken $ concatenate <$> unions
        [filterJust $ update' <$> bTokenSelection <@> eTokenCreate]

    onChanges bDatabaseToken $ \items -> do
        liftIO $ BS.writeFile datastoreToken $ toStrict $ encode items

    ----------------------------------------------------------------------------------
    let datastoreLoan = "data/loan.json"
    databaseLoan <- liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreLoan :: UI (Database Loan)


    (loanCreate, eLoanCreate) <- LoanCreate.setup window bDatabaseLoan bDatabaseUser bDatabaseItem
    (loanDelete, eLoanDelete) <- LoanDelete.setup window bDatabaseLoan bDatabaseUser bDatabaseItem

    (loanCreateNormal, eLoanCreateNormal) <- LoanCreateNormal.setup window bDatabaseLoan bDatabaseUser bDatabaseItem
    (loanDeleteNormal, eLoanDeleteNormal) <- LoanDeleteNormal.setup window bDatabaseLoan bDatabaseUser bDatabaseItem


    bDatabaseLoan <- accumB databaseLoan $ concatenate <$> unions
        [ create <$> eLoanCreate
        , delete <$> eLoanDelete
        , create <$> eLoanCreateNormal
        , delete <$> eLoanDeleteNormal
        ]


    onChanges bDatabaseLoan $ \items -> do
        liftIO $ BS.writeFile datastoreLoan $ toStrict $ encode items
    -------------------------------------------------------------------------------------

    let datastoreUser = "data/user.json"
    databaseUser <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreUser :: UI
            (Database User)

    (userCreate, eUserCreate) <- UserCreate.setup window bDatabaseLoan bDatabaseUser bDatabaseItem
    (userDelete, eUserDelete) <- UserDelete.setup window bDatabaseLoan bDatabaseUser bDatabaseItem

    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions
        [ create <$> eUserCreate
        , delete <$> eUserDelete
        ]


    onChanges bDatabaseUser $ \items -> do
        liftIO $ BS.writeFile datastoreUser $ toStrict $ encode items

    -------------------------------------------------------------------------------------
    let datastoreItem = "data/item.json"
    databaseItem <- liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastoreItem :: UI (Database Item)

    (itemCreate, eItemCreate) <- ItemCreate.setup window bDatabaseLoan bDatabaseUser bDatabaseItem
    (itemDelete, eItemDelete) <- ItemDelete.setup window bDatabaseLoan bDatabaseUser bDatabaseItem

    bDatabaseItem <- accumB databaseItem $ concatenate <$> unions
        [ create <$> eItemCreate
        , delete <$> eItemDelete
        ]

    onChanges bDatabaseItem $ \items -> do
        liftIO $ BS.writeFile datastoreItem $ toStrict $ encode items

    ---------------------------------------------------------------------------

    notDone <- UI.string "Ikke færdig"
    let display y x = if traceShowId y
            then case Tab.name (traceShowId x) of
                "Aflever" -> [tabs, loanDelete]
                "Lån" -> [tabs, loanCreate]
                "Opret vare"    -> [tabs, itemCreate]
                "Slet vare"    -> [tabs, itemDelete]
                "Opret bruger"    -> [tabs, userCreate]
                "Slet bruger"    -> [tabs, userDelete]
                "Aflever (Normal)" -> [tabs, loanDeleteNormal]
                "Lån (Normal)" -> [tabs, loanCreateNormal]
                "Smid til reperation" -> [tabs,notDone]
                "Historik" -> [tabs,notDone]
                "Optælling" -> [tabs,notDone]
                "Eksport/Import" -> [tabs,notDone]
                "Søg" -> [tabs,notDone]
                "Søg Normal" -> [tabs,notDone]
            else [tokenCreate]


--------------------------------------------------------------------------------
    let bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bTokenSelection

        bHasToken :: Behavior Bool
        bHasToken = isJust <$> bSelectedToken

    let bGui = display <$> bHasToken

    content <- UI.div # sink children (maybe [tokenCreate] <$> bGui <*> bItem)
-------------------------------------------------------------------------------

    element content
