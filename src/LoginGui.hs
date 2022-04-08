{-# LANGUAGE RecursiveDo #-}
module LoginGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           User
import           Login                          ( Login(..) )
import qualified Login

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString               as BS

import           Database
import qualified Checkbox



setup :: Window -> UI ()
setup window = void $ mdo
    let datastoreUser  = "data/user.json"
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

    return window # set title "PhotoApp - Login"

    -- GUI elements
    loginBtn                          <- UI.button #+ [string "Login"]
    logoutBtn                         <- UI.button #+ [string "Logout"]

    ((elemName, elemCode), tDataItem) <- dataItem bSelectionDataItem


    -- GUI layout
    let uiDataItems = grid
            [ [ string "Username:"
              , element elemName #. "input"
              , string "Password:"
              , element elemCode #. "input"
              ]
            ]

    getBody window
        #+ [ UI.div
             #. "container"
             #+ [ grid
                      [ [uiDataItems]
                      , [ row
                              [ element loginBtn #. "button"
                              , element logoutBtn #. "button"
                              ]
                        ]
                      ]
                ]
           ]


    let eDataItemIn = rumors $ tDataItem
        eLogin      = UI.click loginBtn
        eLogout     = UI.click logoutBtn


    bDatabaseUser  <- accumB databaseUser $ concatenate <$> unions []


    bDatabaseLogin <- accumB databaseLogin $ concatenate <$> unions
        [ filterJust $ update' <$> bLoginSelection <*> bLogin <@ eLogin
        , filterJust $ update' <$> bLoginSelection <@> eDataItemIn
        , filterJust $ update' <$> bLoginSelection <@> (emptyDataItem <$ eLogout)
        ]

    bLoginSelection <- stepper (Just 0) UI.never

    let bLookupLogin :: Behavior (DatabaseKey -> Maybe Login)
        bLookupLogin = flip lookup <$> bDatabaseLogin

        bSelectionDataItem :: Behavior (Maybe Login)
        bSelectionDataItem = (=<<) <$> bLookupLogin <*> bLoginSelection

        bFind :: Behavior ((User -> Bool) -> Maybe DatabaseKey)
        bFind = flip findIndex <$> bDatabaseUser

        compareLogin :: Login -> User -> Bool
        compareLogin y x = (User.name x == Login.name y) && (User.code x == Login.code y)

        liftOp :: Monad m => (a -> b -> c) -> m a -> b -> m c
        liftOp f a b = a >>= \a' -> return (f a' b)

        loginIsUser :: Behavior (User -> Bool)
        loginIsUser =  (fromMaybe False .) . (liftOp compareLogin) <$> bSelectionDataItem

        bUserKey :: Behavior (Maybe DatabaseKey)
        bUserKey = bFind <*> loginIsUser

        bUser' :: Behavior (Maybe User)
        bUser' = (=<<) <$> bLookupUser <*> bUserKey

        bUser :: Behavior User
        bUser = fromMaybe emptyUser <$> bUser'

        bLogin :: Behavior Login
        bLogin = (\e -> Login (User.name e) (User.code e)) <$> bUser

        bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser


    onChanges bDatabaseLogin $ \items -> do
        liftIO $ putStrLn (show items)
        liftIO $ BS.writeFile datastoreLogin $ toStrict $ encode items

    element logoutBtn -- sink enabled
    element loginBtn -- sink enabled



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
    entry2 <- UI.entry $ Login.code . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , Login <$> UI.userText entry1 <*> UI.userText entry2
        )
