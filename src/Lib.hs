{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where
import qualified Data.Text as T
import           Data.Password.Bcrypt
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
import qualified History.History               as History
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

import           History                           ( History(..) )
import           Loan                           ( Loan(..) )
import           User                           ( User(..) )
import           Token                          ( Token(..) , isToken, tokenId)


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
    let dataTabSelectionFile = "data/tabSelection.json"

    let datastoreLoan = "data/loan.json"
    let datastoreUser = "data/user.json"
    let datastoreToken = "data/token.json"
    let datastoreTab = "data/tab.json"
    let datastoreItem = "data/item.json"
    let datastoreHistory = "data/history.json"
    let exportFile = "data/export.csv"
    (eUserCreate', hUserCreate') <- liftIO $ newEvent
    tabSelectionFile <- return (Just 0)
        {-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile dataTabSelectionFile :: UI (Maybe Int)
        -}

    databaseUser <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreUser :: UI (Database User)
    databaseToken <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreToken :: UI (Database Token)
    databaseTab <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreTab :: UI (Database Tab)
    databaseItem <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreItem :: UI (Database Item)

    databaseLoan <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreLoan :: UI (Database Loan)

    databaseHistory <- 
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreHistory :: UI (Database History)


    notDone <- UI.string "Ikke færdig"

    (export, eExport) <- Export.setup window
                                      bDatabaseLoan
                                      bDatabaseUser
                                      bDatabaseItem
                                      bDatabaseToken
                                      bTokenSelection


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

    history <- History.setup window
                                                     bDatabaseLoan
                                                     bDatabaseUser
                                                     bDatabaseItem
                                                     bDatabaseToken
                                                     bTokenSelection
                                                     bDatabaseHistory


    search <- Search.setup window bDatabaseLoan bDatabaseUser bDatabaseItem


    (tabs, tTabs, eLogout) <- Tab.setup window
                               bDatabaseLoan
                               bDatabaseUser
                               bDatabaseItem
                               bDatabaseToken
                               bTokenSelection
                               bDatabaseTab
                               bTabSelection

    searchNormal <- SearchNormal.setup window
                                       bDatabaseLoan
                                       bDatabaseUser
                                       bDatabaseItem
                                       bDatabaseToken
                                       bTokenSelection

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


    (itemCreate, eItemCreate) <- ItemCreate.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem
    (itemDelete, eItemDelete) <- ItemDelete.setup window
                                                  bDatabaseLoan
                                                  bDatabaseUser
                                                  bDatabaseItem


    (tokenCreate, eTokenCreate) <- TokenCreate.setup window
                                                     bDatabaseLoan
                                                     bDatabaseUser
                                                     bDatabaseItem
                                                     bDatabaseToken
                                                     bTokenSelection


    let eTabs = rumors tTabs

    bDatabaseLoan <- accumB databaseLoan $ concatenate <$> unions
        [ Database.create <$> eLoanCreate
        , Database.delete <$> eLoanDelete
        , Database.create <$> eLoanCreateNormal
        , Database.delete <$> eLoanDeleteNormal
        ]



    bDatabaseItem <- accumB databaseItem $ concatenate <$> unions
        [Database.create <$> eItemCreate, Database.delete <$> eItemDelete]


    bTokenSelection <- stepper (Just 0) UI.never
    bDatabaseToken  <- accumB databaseToken $ concatenate <$> unions
        [filterJust $ Database.update' <$> bTokenSelection <@> eTokenCreate
        ,filterJust $ Database.update' <$> bTokenSelection <@> eLogout
        ]

    bTabSelection <- stepper tabSelectionFile $ Unsafe.head <$> unions [ eTabs, Nothing <$ eLogout, (\b -> if b then Just 6 else Just 0) <$> bSelectedAdmin <@ eTokenCreate]
    bDatabaseTab  <- accumB databaseTab $ concatenate <$> unions []


    bDatabaseExport <- stepper Database.emptydb $ Unsafe.head <$> unions
        [eExport]


    bDatabaseHistory <- accumB databaseHistory $ concatenate <$> unions
        [ Database.create . History <$> eLoanCreate
        , Database.create . History <$> eLoanCreateNormal
        ]
    let bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip Database.lookup <$> bDatabaseLoan


    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions
        [Database.create <$> eUserCreate', Database.delete <$> eUserDelete]

    let display y isAdmin x = if y
            then case (x, isAdmin) of
                (0,True)  -> [tabs, loanCreate]
                (1,True)  -> [tabs, loanDelete]
                (2,True)  -> [tabs, itemCreate]
                (3,True)  -> [tabs, itemDelete]
                (4,True)  -> [tabs, userCreate]
                (5,True)  -> [tabs, userDelete]
                (6,False)  -> [tabs, loanCreateNormal]
                (7,False)  -> [tabs, loanDeleteNormal]
                (8,True)  -> [tabs, search]
                (9,False)  -> [tabs, searchNormal]
                (10,True) -> [tabs, export]
                (11,True) -> [tabs, history]
                (12,True)-> [tabs, notDone]
                (13,True) -> [tabs, notDone]
                (0,False) -> [tabs, loanDeleteNormal]--- Hack
            else [tokenCreate]


    let bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip Database.lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bTokenSelection

        bHasToken :: Behavior Bool
        bHasToken = maybe False Token.isToken <$> bSelectedToken

        bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip Database.lookup <$> bDatabaseUser

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectedTokenId

        bSelectedAdmin :: Behavior Bool
        bSelectedAdmin = (maybe False User.admin ) <$> bSelectedUser

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo tokenId <$> bSelectedToken

    let bGui = display <$> bHasToken <*> bSelectedAdmin


    content <- UI.div
        # sink children (maybe [tokenCreate] <$> bGui <*> bTabSelection)

    getBody window #+ [element content]


    onEvent eUserCreate $ \x -> void $ liftIO $ do
            let password = mkPassword $ T.pack $ User.password x
            passHash <- hashPassword password
            hUserCreate' (User.User (User.name x) (T.unpack $ unPasswordHash passHash) (User.admin x))

    onChanges bDatabaseHistory $ \items -> do
        liftIO $ BS.writeFile datastoreHistory $ toStrict $ encode items

    onChanges bDatabaseLoan $ \items -> do
        liftIO $ BS.writeFile datastoreLoan $ toStrict $ encode items

    onChanges bDatabaseUser $ \items -> do
        liftIO $ BS.writeFile datastoreUser $ toStrict $ encode items

    onChanges bDatabaseItem $ \items -> do
        liftIO $ BS.writeFile datastoreItem $ toStrict $ encode items

    onChanges bDatabaseToken $ \items -> do
        liftIO $ BS.writeFile datastoreToken $ toStrict $ encode items
    onChanges bDatabaseExport $ \db -> do
        let items = Database.elems db
        let csv   = Csv.encodeDefaultOrderedByName items
        liftIO $ BS.writeFile exportFile (toStrict csv)


    onChanges bTabSelection $ \item -> do -- NOT A DB MAYBE CHANGE THAT?
        liftIO $ BS.writeFile dataTabSelectionFile $ toStrict $ encode item

