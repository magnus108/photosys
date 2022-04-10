{-# LANGUAGE RecursiveDo #-}
module LoginGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           User
import           Login                          ( Login(..) )
import qualified Login
import           Token

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString               as BS

import           Database
import qualified Checkbox



setup
    :: Window
    -> UI (Element, (Element, Element), Behavior Bool, Behavior (Maybe User))
setup window = mdo
    let datastoreUser = "data/user.json"
    databaseUser <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreUser :: UI (Database User)

    let datastoreLogin = "data/login.json"
    databaseLogin <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreLogin :: UI (Database Login)

    let datastoreToken = "data/token.json"
    databaseToken <-
        liftIO
        $   Unsafe.fromJust
        .   decode
        .   fromStrict
        <$> BS.readFile datastoreToken :: UI (Database Token)

    return window # set title "PhotoApp - Login"

    -- GUI elements
    loginBtn <- UI.button #+ [string "Login"]
    logoutBtn <- UI.button #+ [string "Logout"]

    ((elemName, elemPassword), tDataItem) <- dataItem bSelectionDataItem

    username <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Username"]
           , UI.div #. "control" #+ [element elemName #. "input"]
           ]

    password <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Username"]
           , UI.div
           #. "control"
           #+ [element elemPassword #. "input" # set UI.type_ "password"]
           ]

    buttons <-
        UI.div
        #. "field is-grouped"
        #+ [UI.div #. "control" #+ [element loginBtn #. "button"]
           ,UI.div #. "control" #+ [element logoutBtn #. "button"]
           ]

    elem <-
        UI.div
        #. "container"
        #+ [element username, element password, element buttons]


    let eDataItemIn = rumors $ tDataItem
        eLogin      = UI.click loginBtn
        eLogout     = UI.click logoutBtn


    bDatabaseUser  <- accumB databaseUser $ concatenate <$> unions []


    bDatabaseLogin <- accumB databaseLogin $ concatenate <$> unions
        [ filterJust $ update' <$> bLoginSelection <*> bLogin <@ eLogin
        , filterJust $ update' <$> bLoginSelection <@> eDataItemIn
        , filterJust
        $   update'
        <$> bLoginSelection
        <@> (emptyDataItem <$ eLogout)
        ]

    bLoginSelection <- stepper (Just 0) UI.never


    bDatabaseToken  <- accumB databaseToken $ concatenate <$> unions
        [ filterJust
        $   update'
        <$> bTokenSelection
        <@> (Token <$ filterJust (bUser' <@ eLogin))
        , filterJust $ update' <$> bTokenSelection <@> (NoToken <$ eLogout)
        ]

    bTokenSelection <- stepper (Just 0) UI.never


    let bLookupLogin :: Behavior (DatabaseKey -> Maybe Login)
        bLookupLogin = flip lookup <$> bDatabaseLogin

        bSelectionDataItem :: Behavior (Maybe Login)
        bSelectionDataItem = (=<<) <$> bLookupLogin <*> bLoginSelection

        bFind :: Behavior ((User -> Bool) -> Maybe DatabaseKey)
        bFind = flip findIndex <$> bDatabaseUser

        compareLogin :: Login -> User -> Bool
        compareLogin y x =
            (User.name x == Login.name y)
                && (User.password x == Login.password y)

        liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c
        liftOp f a b = a >>= \a' -> return (f a' b)

        loginIsUser :: Behavior (User -> Bool)
        loginIsUser =
            (fromMaybe False .) . (liftOp compareLogin) <$> bSelectionDataItem

        bUserKey :: Behavior (Maybe DatabaseKey)
        bUserKey = bFind <*> loginIsUser

        bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bUser' :: Behavior (Maybe User)
        bUser' = (=<<) <$> bLookupUser <*> bUserKey

        bUser :: Behavior User
        bUser = fromMaybe emptyUser <$> bUser'

        bLogin :: Behavior Login
        bLogin = (\e -> Login (User.name e) (User.password e)) <$> bUser


    let bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectionToken :: Behavior (Maybe Token)
        bSelectionToken = (=<<) <$> bLookupToken <*> bTokenSelection


    onChanges bDatabaseLogin $ \items -> do
        liftIO $ putStrLn (show items)
        liftIO $ BS.writeFile datastoreLogin $ toStrict $ encode items

    onChanges bDatabaseToken $ \items -> do
        liftIO $ putStrLn (show items)
        liftIO $ BS.writeFile datastoreToken $ toStrict $ encode items

    element elemName # sink
        value
        (Login.name . fromMaybe emptyDataItem <$> bSelectionDataItem)
    element elemPassword # sink
        value
        (Login.password . fromMaybe emptyDataItem <$> bSelectionDataItem)

    let bDisplayItem :: Behavior Bool
        bDisplayItem = maybe False isToken <$> bSelectionToken

    element loginBtn # sink UI.enabled (not <$> bDisplayItem)
    element logoutBtn # sink UI.enabled bDisplayItem

    return (elem, (loginBtn, logoutBtn), bDisplayItem, bUser')



{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}


type DataItem = Login

emptyDataItem :: DataItem
emptyDataItem = Login "" ""

emptyUser :: User
emptyUser = User "" "" False


dataItem
    :: Behavior (Maybe DataItem) -> UI ((Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ Login.name . fromMaybe emptyDataItem <$> bItem
    entry2 <- UI.entry $ Login.password . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , Login <$> UI.userText entry1 <*> UI.userText entry2
        )
