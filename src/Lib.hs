{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where
import qualified Data.Csv                      as Csv
import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe
import           Database                       ( Database
                                                , DatabaseKey
                                                )
import qualified Database

import qualified MenuBox
import qualified Export.Export                 as Export
import qualified Tab.Tab                       as Tab

import qualified Item.Create                   as ItemCreate
import qualified Item.Delete                   as ItemDelete

import qualified Loan.Create                   as LoanCreate
import qualified Loan.Delete                   as LoanDelete
import qualified Search.Search                 as Search
import qualified Search.SearchNormal           as SearchNormal

import qualified Loan.CreateNormal             as LoanCreateNormal
import qualified Loan.DeleteNormal             as LoanDeleteNormal


import qualified User.Create                   as UserCreate
import qualified User.Delete                   as UserDelete

import qualified Token.Create                  as TokenCreate

import           Loan                           ( Loan(..) )
import           User                           ( User(..) )
import           Token                          ( Token(..) , isToken)


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
    let datastoreLoan = "data/loan.json"
    databaseLoan <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreLoan :: UI (Database Loan)


    (loanCreate, eLoanCreate) <- LoanCreate.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem
                                                  bDatabaseToken
                                                  bTokenSelection
    (loanDelete, eLoanDelete) <- LoanDelete.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem

    (loanCreateNormal, eLoanCreateNormal) <- LoanCreateNormal.setup
        window
        bDatabaseLoan
        bDatabaseUser
        bDatabaseItem
        bDatabaseToken
        bTokenSelection
    (loanDeleteNormal, eLoanDeleteNormal) <- LoanDeleteNormal.setup
        window
        bDatabaseLoan
        bDatabaseUser
        bDatabaseItem
        bDatabaseToken
        bTokenSelection

    search <- Search.setup window bDatabaseLoan bDatabaseUser bDatabaseItem
    searchNormal <- SearchNormal.setup window
                                       bDatabaseLoan
                                       bDatabaseUser
                                       bDatabaseItem
                                       bDatabaseToken
                                       bTokenSelection


    bDatabaseLoan <- accumB databaseLoan $ concatenate <$> unions
        [ Database.create <$> eLoanCreate
        , Database.delete <$> eLoanDelete
        , Database.create <$> eLoanCreateNormal
        , Database.delete <$> eLoanDeleteNormal
        ]


    onChanges bDatabaseLoan $ \items -> do
        liftIO $ BS.writeFile datastoreLoan $ toStrict $ encode items
    -------------------------------------------------------------------------------------

    let datastoreUser = "data/user.json"
    databaseUser <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreUser :: UI (Database User)

    (userCreate, eUserCreate) <- UserCreate.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem
    (userDelete, eUserDelete) <- UserDelete.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem
                                                  bDatabaseToken
                                                  bTokenSelection

    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions
        [Database.create <$> eUserCreate, Database.delete <$> eUserDelete]


    onChanges bDatabaseUser $ \items -> do
        liftIO $ BS.writeFile datastoreUser $ toStrict $ encode items

    -------------------------------------------------------------------------------------
    let datastoreItem = "data/item.json"
    databaseItem <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreItem :: UI (Database Item)

    (itemCreate, eItemCreate) <- ItemCreate.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem
    (itemDelete, eItemDelete) <- ItemDelete.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem

    bDatabaseItem <- accumB databaseItem $ concatenate <$> unions
        [Database.create <$> eItemCreate, Database.delete <$> eItemDelete]

    onChanges bDatabaseItem $ \items -> do
        liftIO $ BS.writeFile datastoreItem $ toStrict $ encode items

    ----------------------------------------------------------------------------------
    let datastoreTab = "data/tab.json"
    databaseTab <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreTab :: UI (Database Tab)

    (tabs, tTabs, eLogout) <- Tab.setup window
                               bDatabaseLoan
                               bDatabaseUser
                               bDatabaseItem
                               bDatabaseToken
                               bTokenSelection
                               bDatabaseTab
                               bTabSelection

    let eTabs = rumors tTabs
    bTabSelection <- stepper (Just 0) $ Unsafe.head <$> unions [ eTabs ]
    bDatabaseTab  <- accumB databaseTab $ concatenate <$> unions []

    ----------------------------------------------------------------------------------

    let datastoreToken = "data/token.json"
    databaseToken <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreToken :: UI (Database Token)

    (tokenCreate, eTokenCreate) <- TokenCreate.setup window
                                                     bDatabaseLoan
                                                     bDatabaseUser
                                                     bDatabaseItem
                                                     bDatabaseToken
                                                     bTokenSelection

    bTokenSelection <- stepper (Just 0) UI.never
    bDatabaseToken  <- accumB databaseToken $ concatenate <$> unions
        [filterJust $ Database.update' <$> bTokenSelection <@> eTokenCreate
        ,filterJust $ Database.update' <$> bTokenSelection <@> eLogout
        ]

    onChanges bDatabaseToken $ \items -> do
        liftIO $ BS.writeFile datastoreToken $ toStrict $ encode items

    ----------------------------------------------------------------------------------

    ---------------------------------------------------------------------------
    -- EXPORT
    let exportFile = "data/export.csv"
    (export, eExport) <- Export.setup window
                                      bDatabaseLoan
                                      bDatabaseUser
                                      bDatabaseItem
                                      bDatabaseToken
                                      bTokenSelection

    bDatabaseExport <- stepper Database.emptydb $ Unsafe.head <$> unions
        [eExport]

    onChanges bDatabaseExport $ \db -> do
        let items = Database.elems db
        let csv   = Csv.encodeDefaultOrderedByName items
        liftIO $ BS.writeFile exportFile (toStrict csv)
    ---------------------------------------------------------------------------



    notDone <- UI.string "Ikke færdig"
    let display y x = if traceShowId y
            then case x of
                0  -> [tabs, loanCreate]
                1  -> [tabs, loanDelete]
                2  -> [tabs, itemCreate]
                3  -> [tabs, itemDelete]
                4  -> [tabs, userCreate]
                5  -> [tabs, userDelete]
                6  -> [tabs, loanCreateNormal]
                7  -> [tabs, loanDeleteNormal]
                8  -> [tabs, search]
                9  -> [tabs, searchNormal]
                10 -> [tabs, export]
                11 -> [tabs, notDone]
                12 -> [tabs, notDone]
                13 -> [tabs, notDone]
            else [tokenCreate]


--------------------------------------------------------------------------------
    let bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip Database.lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bTokenSelection

        bHasToken :: Behavior Bool
        bHasToken = maybe False Token.isToken <$> bSelectedToken

    let bGui = display <$> bHasToken

    content <- UI.div
        # sink children (maybe [tokenCreate] <$> bGui <*> bTabSelection)

    getBody window #+ [element content]

