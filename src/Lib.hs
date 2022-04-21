{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where
import           Env
import           Monad
import qualified Data.Text                     as T
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
import qualified History.HistoryNormal         as HistoryNormal
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

import           History                        ( History(..) )
import           Loan                           ( Loan(..) )
import           User                           ( User(..) )
import           Token                          ( Token(..)
                                                , isToken
                                                , tokenId
                                                )


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
        $ setup2



readJson :: (MonadIO m, FromJSON a) => FilePath -> m a
readJson fp = liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile fp

writeJson :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJson fp items = liftIO $ BS.writeFile fp $ toStrict $ encode items

writeCsv
    :: (MonadIO m, Csv.DefaultOrdered a, Csv.ToNamedRecord a)
    => FilePath
    -> [a]
    -> m ()
writeCsv fp items =
    liftIO $ BS.writeFile fp $ toStrict $ Csv.encodeDefaultOrderedByName items



setup2 :: Window -> UI ()
setup2 window = void $ mdo
    env <- runApp env $ setup window
    return ()

setup
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m Env
setup window = mdo

    let dataTabSelectionFile = "data/tabSelection.json"
    let datastoreLoan        = "data/loan.json"
    let datastoreUser        = "data/user.json"
    let datastoreToken       = "data/token.json"
    let datastoreTab         = "data/tab.json"
    let datastoreItem        = "data/item.json"
    let datastoreHistory     = "data/history.json"
    let exportFile           = "data/export.csv"

    tabSelectionFile <- readJson dataTabSelectionFile :: m (Maybe Int)
    databaseUser <- readJson datastoreUser :: m (Database User)
    databaseToken <- readJson datastoreToken :: m (Database Token)
    databaseTab <- readJson datastoreTab :: m (Database Tab)
    databaseItem <- readJson datastoreItem :: m (Database Item)
    databaseLoan <- readJson datastoreLoan :: m (Database Loan)
    databaseHistory <- readJson datastoreHistory :: m (Database History)

    (export, eExport) <- Export.setup window
    (loanCreate, eLoanCreate) <- LoanCreate.setup window
    (loanDelete, eLoanDelete) <- LoanDelete.setup window
    (loanCreateNormal, eLoanCreateNormal) <- LoanCreateNormal.setup window
    (loanDeleteNormal, eLoanDeleteNormal) <- LoanDeleteNormal.setup window
    history <- History.setup window
    historyNormal <- HistoryNormal.setup window
    search <- Search.setup window
    (tabs, tTabs, eLogout) <- Tab.setup window
    searchNormal <- SearchNormal.setup window
    (userCreate, eUserCreate) <- UserCreate.setup window
    (userDelete, eUserDelete) <- UserDelete.setup window
    (itemCreate, eItemCreate) <- ItemCreate.setup window
    (itemDelete, eItemDelete) <- ItemDelete.setup window
    (tokenCreate, eTokenCreate) <- TokenCreate.setup window


    let eTabs = rumors tTabs

    bDatabaseLoan <- accumB databaseLoan $ concatenate <$> unions
        [ Database.create <$> eLoanCreate
        , Database.delete <$> eLoanDelete
        , Database.create <$> eLoanCreateNormal
        , Database.delete <$> eLoanDeleteNormal
        ]

    bDatabaseItem <- accumB databaseItem $ concatenate <$> unions
        [Database.create <$> eItemCreate, Database.delete <$> eItemDelete]


    bTabSelection <- stepper tabSelectionFile $ Unsafe.head <$> unions
        [ eTabs
        , Nothing <$ eLogout
        , (\b -> if b then Just 6 else Just 0)
        <$> bSelectedAdmin
        <@  eTokenCreate
        ]

    bDatabaseTab     <- accumB databaseTab $ concatenate <$> unions []

    bDatabaseExport  <- stepper [] $ Unsafe.head <$> unions [eExport]

    bDatabaseHistory <- accumB databaseHistory $ concatenate <$> unions
        [ Database.create . History <$> eLoanCreate
        , Database.create . History <$> eLoanCreateNormal
        ]


    let bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip Database.lookup <$> bDatabaseLoan

    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions
        [ Database.create
            <$> unsafeMapIO
                    (\x -> do
                        let password = mkPassword $ T.pack $ User.password x
                        passHash <- hashPassword password
                        return $ User.User
                            (User.name x)
                            (T.unpack $ unPasswordHash passHash)
                            (User.admin x)
                    )
                    eUserCreate
        , Database.delete <$> eUserDelete
        ]


    bTokenSelection <- stepper (Just 0) UI.never

    bDatabaseToken  <- accumB databaseToken $ concatenate <$> unions
        [ filterJust $ Database.update' <$> bTokenSelection <@> eTokenCreate
        , filterJust $ Database.update' <$> bTokenSelection <@> eLogout
        ]


    let env = Env { bDatabaseLoan    = bDatabaseLoan
                  , bDatabaseUser    = bDatabaseUser
                  , bDatabaseItem    = bDatabaseItem
                  , bDatabaseToken   = bDatabaseToken
                  , bSelectionToken  = bTokenSelection
                  , bDatabaseHistory = bDatabaseHistory
                  , bDatabaseTab     = bDatabaseTab
                  , bSelectionTab    = bTabSelection
                  }

    notDone <- liftUI $ UI.string "Ikke færdig"

    let display y isAdmin x = if y
            then case (x, isAdmin) of
                (0 , True ) -> [tabs, loanCreate]
                (1 , True ) -> [tabs, loanDelete]
                (2 , True ) -> [tabs, itemCreate]
                (3 , True ) -> [tabs, itemDelete]
                (4 , True ) -> [tabs, userCreate]
                (5 , True ) -> [tabs, userDelete]
                (6 , False) -> [tabs, loanCreateNormal]
                (7 , False) -> [tabs, loanDeleteNormal]
                (8 , True ) -> [tabs, search]
                (9 , False) -> [tabs, searchNormal]
                (10, True ) -> [tabs, export]
                (11, True ) -> [tabs, history]
                (12, False) -> [tabs, historyNormal]
                (13, True ) -> [tabs, notDone]
                (14, True ) -> [tabs, notDone]
                (0 , False) -> [tabs, loanDeleteNormal]--- Hack
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
        bSelectedAdmin = (maybe False User.admin) <$> bSelectedUser

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo tokenId <$> bSelectedToken

    let bGui = display <$> bHasToken <*> bSelectedAdmin

    content <- liftUI $ UI.div # sink
        children
        (maybe [tokenCreate] <$> bGui <*> bTabSelection)


    liftUI $ getBody window #+ [element content]

    liftUI $ onChanges bDatabaseHistory $ writeJson datastoreHistory

    liftUI $ onChanges bDatabaseLoan $ writeJson datastoreLoan

    liftUI $ onChanges bDatabaseUser $ writeJson datastoreUser

    liftUI $ onChanges bDatabaseItem $ writeJson datastoreItem

    liftUI $ onChanges bDatabaseToken $ writeJson datastoreToken

    liftUI $ onChanges bDatabaseExport $ writeCsv exportFile

    liftUI $ onChanges bTabSelection $ writeJson dataTabSelectionFile

    return env
