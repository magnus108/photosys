{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where
import           Data.Time
import           Changes
import           Config
import           Utils.Data
import           Env
import           Monad
import qualified Data.Text                     as T
import           Data.Password.Bcrypt
import qualified Data.Csv                      as Csv
import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete
                                                , Config
                                                )

import           Item

import qualified Relude.Unsafe                 as Unsafe
import           Database                       ( Database
                                                , DatabaseKey
                                                )
import qualified Database

import qualified MenuBox
import qualified Timer
import qualified History.HistoryNormal         as HistoryNormal
import qualified History.History               as History
import qualified History.HistoryHandinNormal   as HistoryHandinNormal
import qualified History.HistoryHandin         as HistoryHandin
import qualified Export.Export                 as Export
import qualified Tab.Tab                       as Tab

import qualified Repair.Repair                 as Repair
import qualified Repair.RepairCreate                 as RepairCreate
import qualified Repair.RepairCreateNormal                 as RepairCreateNormal

import qualified Item.Create                   as ItemCreate
import qualified Item.Delete                   as ItemDelete

import qualified Loan.Create                   as LoanCreate
import qualified Loan.Delete                   as LoanDelete
import qualified Search.Search                 as Search
import qualified Search.SearchNormal           as SearchNormal

import qualified Loan.CreateNormal             as LoanCreateNormal
import qualified Loan.DeleteNormal             as LoanDeleteNormal
import qualified Count.Count                   as Count


import qualified User.Create                   as UserCreate
import qualified User.Delete                   as UserDelete

import qualified Token.Create                  as TokenCreate

import           HistoryHandin                  ( HistoryHandin(..) )
import           History                        ( History(..) )
import           Loan                           ( Loan(..) )
import           Loan                           ( Loan(..) )
import           User                           ( User(..) )
import           Count                          ( Count(..) )
import           Time                           ( Time(..) )
import           Repair                         ( Repair(..) )
import           Token                          ( Token(..)
                                                , isToken
                                                , tokenTTL
                                                , tokenId
                                                )


import           Tab                            ( Tab(..) )
import qualified Tab

import qualified Data.ByteString               as BS


someFunc :: Int -> IO ()
someFunc port = do

    let config = Config { dataTabSelectionFile   = "data/tabSelection.json"
                        , datastoreLoan          = "data/loan.json"
                        , datastoreUser          = "data/user.json"
                        , datastoreToken         = "data/token.json"
                        , datastoreTab           = "data/tab.json"
                        , datastoreItem          = "data/item.json"
                        , datastoreHistory       = "data/history.json"
                        , datastoreHistoryHandIn = "data/historyHandin.json"
                        , datastoreCount         = "data/count.json"
                        , datastoreTime          = "data/time.json"
                        , datastoreRepair        = "data/repair.json"
                        , exportFile             = "data/export.csv"
                        }

    startGUI defaultConfig { jsPort       = Just port
                           , jsStatic     = Just "static"
                           , jsCustomHTML = Just "index.html"
                           }
        $ setup2 config





setup2 :: Config -> Window -> UI ()
setup2 config@Config {..} window = void $ mdo

    (anyE, anyH) <- liftIO $ newEvent

    env          <- runApp env $ setup config window (anyE, anyH)

    return window # set title "Lager FF"

    runApp env $ do
        changesHistory datastoreHistory anyH
        changesHistoryHandin datastoreHistoryHandIn anyH
        changesLoan datastoreLoan anyH
        changesUser datastoreUser anyH
        changesItem datastoreItem anyH
        changesToken datastoreToken anyH
        changesCount datastoreCount anyH
        changesTime datastoreTime anyH
        changesRepair datastoreRepair anyH

    return ()

setup
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Config
    -> Window
    -> (Event Token, Handler Token)
    -> m Env
setup Config {..} window (anyE, anyH) = mdo

    tabSelectionFile      <- readJson dataTabSelectionFile :: m (Maybe Int)
    databaseUser          <- readJson datastoreUser :: m (Database User)
    databaseToken         <- readJson datastoreToken :: m (Database Token)
    databaseTab           <- readJson datastoreTab :: m (Database Tab)
    databaseItem          <- readJson datastoreItem :: m (Database Item)
    databaseLoan          <- readJson datastoreLoan :: m (Database Loan)
    databaseHistory       <- readJson datastoreHistory :: m (Database History)
    databaseHistoryHandin <-
        readJson datastoreHistoryHandIn :: m (Database HistoryHandin)
    databaseCount <- readJson datastoreCount :: m (Database Count)
    databaseTime <- readJson datastoreTime :: m (Database Time)
    databaseRepair <- readJson datastoreRepair :: m (Database Repair)

-------------------------------------------------------------------------------

    (tokenCreate, eTokenCreate)            <- TokenCreate.setup window
    (tabs, tTabs, eLogout)                 <- Tab.setup window
    (export, eExport)                      <- Export.setup window
    ce <- LoanCreate.setup window
    de <- LoanDelete.setup window
    (loanCreateNormal, eLoanCreateNormal)  <- LoanCreateNormal.setup window
    (loanDeleteNormal, eLoanDeleteNormal)  <- LoanDeleteNormal.setup window
    history                                <- History.setup window
    historyNormal                          <- HistoryNormal.setup window

    (historyHandin, tHistoryHandinLoan, tHistoryHandinUser, tHistoryHandinItem, tHistoryHandinFilterUser) <-
        HistoryHandin.setup window
    historyHandinNormal           <- HistoryHandinNormal.setup window

    (count, eCount, eCountDelete) <- Count.setup window
    search                        <- Search.setup window
    searchNormal                  <- SearchNormal.setup window
    (userCreate, eUserCreate)     <- UserCreate.setup window
    (userDelete, eUserDelete, ePassUpdate)     <- UserDelete.setup window
    (itemCreate, eItemCreate, eItemChange)     <- ItemCreate.setup window
    (itemDelete, eItemDelete)     <- ItemDelete.setup window

    (repairCreate , eRepairCreate )     <- RepairCreate.setup window
    (repairCreateNormal , eRepairCreateNormal)     <- RepairCreateNormal.setup window
    (repair    , eRepair    )     <- Repair.setup window

    (eTime)                       <- Timer.setup window


    notDone                       <- liftUI $ UI.string "Ikke færdig"
    content                       <- liftUI $ UI.div

    liftUI $ getBody window #+ [element content]

    let eHistoryHandinItem       = rumors tHistoryHandinItem
    let eHistoryHandinLoan       = rumors tHistoryHandinLoan
    let eHistoryHandinUser       = rumors tHistoryHandinUser
    let eHistoryHandinFilterUser = rumors tHistoryHandinFilterUser

    let allowedDiff              = 600
-------------------------------------------------------------------------------
    let eTabs                    = rumors tTabs
    let
        eAutoLog = unsafeMapIO
            (\_ -> do
                token <- currentValue bSelectedToken
                time  <- currentValue bSelectedTime
                y <- mapM Timer.readTime $ Time.time <$> (tokenTTL =<< token)
                x     <- mapM Timer.readTime $ Time.time <$> time
                let
                    difftime = liftA2 diffUTCTime
                                      (zonedTimeToUTC <$> x)
                                      (zonedTimeToUTC <$> y)
                return $ (\t -> t > allowedDiff) <$> difftime
            )
            eTime
    let eAutoLog' =
            filterJust
                $   (\e -> if e then Just NoToken else Nothing)
                <$> filterJust eAutoLog
-------------------------------------------------------------------------------

    bHistoryHandinLoan       <- stepper Nothing eHistoryHandinLoan
    bHistoryHandinUser       <- stepper Nothing eHistoryHandinUser
    bHistoryHandinFilterUser <- stepper "" eHistoryHandinFilterUser
    bHistoryHandinItem       <- stepper Nothing eHistoryHandinItem

    bTimeSelection           <- stepper (Just 0) UI.never
    bDatabaseTime            <- accumB databaseTime $ concatenate <$> unions
        [filterJust $ Database.update' <$> bTimeSelection <@> eTime]



    bDatabaseRepair <- accumB databaseRepair $ concatenate <$> unions
        [Database.create <$> eRepairCreate, Database.create <$> eRepairCreateNormal, Database.delete <$> eRepair]

    bDatabaseCount <- accumB databaseCount $ concatenate <$> unions
        [Database.create . Count <$> eCount, Database.delete <$> eCountDelete]

    bDatabaseLoan <- accumB databaseLoan $ concatenate <$> unions
        [ Database.create <$> (LoanCreate._eConfirmLoan ce)
        , Database.delete <$> (LoanDelete._eDeleteLoan de)
        , Database.create <$> eLoanCreateNormal
        , Database.delete <$> eLoanDeleteNormal
        ]

    bDatabaseItem <- accumB databaseItem $ concatenate <$> unions
        [Database.create <$> eItemCreate, eItemChange, Database.delete <$> eItemDelete]


    bTabSelection <- stepper tabSelectionFile $ Unsafe.head <$> unions
        [ eTabs
        , (\b -> if b then Just 6 else Just 0)
        <$> bSelectedAdmin
        <@  eTokenCreate
        ]


    bDatabaseTab     <- accumB databaseTab $ concatenate <$> unions []

    bDatabaseExport  <- stepper [] $ Unsafe.head <$> unions [eExport]

    bDatabaseHistory <- accumB databaseHistory $ concatenate <$> unions
        [ Database.create
            <$> (((\x z y -> History y (fromMaybe (Time "") x) (fromMaybe 999 z)
                  )
                 <$> bSelectedTime
                 <*> bSelectedTokenId
                 )
                <@> (LoanCreate._eConfirmLoan ce)
                )
        , Database.create
            <$> (((\x z y -> History y (fromMaybe (Time "") x) (fromMaybe 999 z)
                  )
                 <$> bSelectedTime
                 <*> bSelectedTokenId
                 )
                <@> eLoanCreateNormal
                )
        ]

    bDatabaseHistoryHandin <-
        accumB databaseHistoryHandin $ concatenate <$> unions
            [ Database.create
                <$> (   (   (\x z y -> HistoryHandin y
                                                     (fromMaybe (Time "") x)
                                                     (fromMaybe 999 z)
                            )
                        <$> bSelectedTime
                        <*> bSelectedTokenId
                        )
                    <@> (filterJust $ bLookupLoan <@> (LoanDelete._eDeleteLoan de))
                    )
            , Database.create
                <$> (   (   (\x z y -> HistoryHandin y
                                                     (fromMaybe (Time "") x)
                                                     (fromMaybe 999 z)
                            )
                        <$> bSelectedTime
                        <*> bSelectedTokenId
                        )
                    <@> (filterJust $ bLookupLoan <@> eLoanDeleteNormal)
                    )
            ]


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
        , ePassUpdate
        , Database.delete <$> eUserDelete
        ]


    bCreateLoanModalState <- stepper False $ Unsafe.head <$> unions
        [rumors (LoanCreate._modalStateCE ce)]

    bCreateLoanFilterUser <- stepper "" $ Unsafe.head <$> unions
        [rumors (LoanCreate._userFilterCE ce)]

    bCreateLoanFilterItem <- stepper "" $ Unsafe.head <$> unions
        [rumors (LoanCreate._itemFilterCE ce)]

    bCreateLoanSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
        [rumors (LoanCreate._userSelectionCE ce), Nothing <$ eTabs]

    bCreateLoanSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [rumors (LoanCreate._itemSelectionCE ce), Nothing <$ eTabs]

    bDeleteLoanFilterUser <- stepper "" $ Unsafe.head <$> unions
        [rumors (LoanDelete._userFilterDE de)]

    bDeleteLoanFilterItem <- stepper "" $ Unsafe.head <$> unions
        [rumors (LoanDelete._itemFilterDE de)]

    bDeleteLoanSelectionUser <- stepper Nothing $ Unsafe.head <$> unions
        [rumors (LoanDelete._userSelectionDE de), Nothing <$ eTabs]

    bDeleteLoanSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [rumors (LoanDelete._itemSelectionDE de), Nothing <$ eTabs]


-------------------------------------------------------------------------------
    let index = 0
    bTokenSelection <- stepper (Just index) UI.never

    now <- liftIO $ (formatTime defaultTimeLocale "%F, %T") <$> getZonedTime
    let
        checkedB db = do
            let token = Database.lookup index db
            y <- mapM Timer.readTime $ Time.time <$> (tokenTTL =<< token)
            x <- mapM Timer.readTime $ Time.time <$> (Just (Time now))
            let
                difftime = liftA2 diffUTCTime
                                  (zonedTimeToUTC <$> x)
                                  (zonedTimeToUTC <$> y)
            return
                $   (\t -> if t > allowedDiff
                        then Database.update index NoToken db
                        else db
                    )
                <$> difftime
    databaseToken' <- liftIO $ checkedB databaseToken
-------------------------------------------------------------------------------

    bDatabaseToken <-
        accumB (fromMaybe databaseToken databaseToken') $ concatenate <$> unions
            [ filterJust $ Database.update' <$> bTokenSelection <@> eTokenCreate
            , filterJust $ Database.update' <$> bTokenSelection <@> eLogout
            , filterJust $ Database.update' <$> bTokenSelection <@> eAutoLog'
            , filterJust $ Database.update' <$> bTokenSelection <@> anyE
            ]

-------------------------------------------------------------------------------

    let env = Env { bDatabaseLoan            = bDatabaseLoan
                  , bDatabaseUser            = bDatabaseUser
                  , bDatabaseItem            = bDatabaseItem
                  , bDatabaseToken           = bDatabaseToken
                  , bSelectionToken          = bTokenSelection
                  , bDatabaseHistory         = bDatabaseHistory
                  , bDatabaseHistoryHandin   = bDatabaseHistoryHandin
                  , bDatabaseTab             = bDatabaseTab
                  , bSelectionTab            = bTabSelection
                  , bDatabaseCount           = bDatabaseCount
                  , bDatabaseTime            = bDatabaseTime
                  , bDatabaseRepair            = bDatabaseRepair
                  , bSelectionTime           = bTimeSelection
                  , bHistoryHandinLoan       = bHistoryHandinLoan
                  , bHistoryHandinUser       = bHistoryHandinUser
                  , bHistoryHandinFilterUser = bHistoryHandinFilterUser
                  , bHistoryHandinItem       = bHistoryHandinItem

                  , bCreateLoanFilterUser = bCreateLoanFilterUser
                  , bCreateLoanFilterItem = bCreateLoanFilterItem
                  , bCreateLoanSelectionUser = bCreateLoanSelectionUser
                  , bCreateLoanSelectionItem = bCreateLoanSelectionItem
                  , bCreateLoanModalState = bCreateLoanModalState

                  , bDeleteLoanFilterUser = bDeleteLoanFilterUser
                  , bDeleteLoanFilterItem = bDeleteLoanFilterItem
                  , bDeleteLoanSelectionUser = bDeleteLoanSelectionUser
                  , bDeleteLoanSelectionItem = bDeleteLoanSelectionItem
                  }

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

    let bLookupLoan :: Behavior (DatabaseKey -> Maybe Loan)
        bLookupLoan = flip Database.lookup <$> bDatabaseLoan

        bLookupTime :: Behavior (DatabaseKey -> Maybe Time)
        bLookupTime = flip Database.lookup <$> bDatabaseTime

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip Database.lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bTokenSelection

        bHasToken :: Behavior Bool
        bHasToken = maybe False Token.isToken <$> bSelectedToken

        bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip Database.lookup <$> bDatabaseUser

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectedTokenId

        bSelectedTime :: Behavior (Maybe Time)
        bSelectedTime = (=<<) <$> bLookupTime <*> bTimeSelection

        bSelectedAdmin :: Behavior Bool
        bSelectedAdmin = (maybe False User.admin) <$> bSelectedUser

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo tokenId <$> bSelectedToken

    let bGui = display <$> bHasToken <*> bSelectedAdmin

-------------------------------------------------------------------------------
    let display y isAdmin x = if y
            then case (x, isAdmin) of
                (0 , True ) -> [tabs, getElement ce]
                (1 , True ) -> [tabs, getElement de]
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
                (13, True ) -> [tabs, historyHandin]
                (14, False) -> [tabs, historyHandinNormal]
                (15, True ) -> [tabs, count]
                (16, True ) -> [tabs, repairCreate]
                (17, True ) -> [tabs, repair]
                (18, False) -> [tabs, repairCreateNormal ]
                (0 , False) -> [tabs, loanDeleteNormal]--- Hack
            else [tokenCreate]

--------------------------------------------------------------------------------
    liftUI $ element content # sink
        children
        (maybe [tokenCreate] <$> bGui <*> bTabSelection)
--------------------------------------------------------------------------------

    let
        updateToken = do
            token <- currentValue bSelectedToken :: IO (Maybe Token)
            time  <- currentValue bSelectedTime
            return $ case token of
                Nothing            -> Nothing
                Just Token.NoToken -> Nothing
                Just (Token.Token a _) ->
                    Just $ Token.Token a (fromMaybe (Time.Time "") time)


    liftUI $ onChanges bTabSelection $ \x -> do
        mt <- liftIO updateToken
        case mt of
            Nothing -> return ()
            Just t  -> liftIO $ anyH t
        writeJson dataTabSelectionFile x


    liftUI $ onChanges bDatabaseExport $ \x -> do
        mt <- liftIO updateToken
        case mt of
            Nothing -> return ()
            Just t  -> liftIO $ anyH t
        writeCsv exportFile x

    return env


