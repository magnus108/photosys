{-# LANGUAGE RecursiveDo #-}
module LoginGui where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           User
import           Login

import qualified Relude.Unsafe                 as Unsafe

import qualified Data.ByteString as BS

import           Database
import qualified Checkbox



setup :: Window -> UI ()
setup window = void $ mdo
    let datastore = "data/user.json"
    databaseUser <-
        liftIO $ Unsafe.fromJust . decode . fromStrict <$> BS.readFile datastore :: UI
            (Database User)

    return window # set title "PhotoApp - Login"

    -- GUI elements
    loginBtn                          <- UI.button #+ [string "Login"]
    logoutBtn                         <- UI.button #+ [string "Logout"]

    ((elemName, elemCode), tDataItem) <- dataItem bLogin


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


    bDatabaseUser <- accumB databaseUser $ concatenate <$> unions []

    bLogin <- stepper Nothing $ Unsafe.head <$> unions
        [ Just <$> eDataItemIn
        , Nothing <$ eLogout
        ]


    bSelection    <- stepper Nothing $ Unsafe.head <$> unions
        [ Nothing <$ eLogout
        , (\f u -> f (\x -> trace (name x) (name x) == traceShow (name_ u) (name_ u) && (code x) == (code_ u))) <$> bFind <*> (fromMaybe emptyDataItem <$> bLogin) <@ eLogin
        ]

    let bLookup :: Behavior (DatabaseKey -> Maybe User)
        bLookup = flip lookup <$> bDatabaseUser

        bFind :: Behavior ((User -> Bool) -> Maybe DatabaseKey)
        bFind = flip findIndex <$> bDatabaseUser

        bSelectionDataItem :: Behavior (Maybe User)
        bSelectionDataItem = (=<<) <$> bLookup <*> bSelection


    element logoutBtn -- sink enabled
    element loginBtn -- sink enabled

    onChanges bSelection $ \items -> do
        liftIO $ putStrLn (show items)

    onChanges bLogin $ \items -> do
        liftIO $ putStrLn ("blogin")
        liftIO $ putStrLn (show items)

    onChanges (facts tDataItem) $ \items -> do
        liftIO $ putStrLn ("facts")
        liftIO $ putStrLn (show items)


{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}

data LoginForm = LoginForm { name_ :: String, code_ :: String }
    deriving Show

type DataItem = LoginForm

emptyDataItem :: DataItem
emptyDataItem = LoginForm "" ""

emptyUser :: User
emptyUser = User "" "" False


dataItem
    :: Behavior (Maybe DataItem) -> UI ((Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ name_ . fromMaybe emptyDataItem <$> bItem
    entry2 <- UI.entry $ code_ . fromMaybe emptyDataItem <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , LoginForm <$> UI.userText entry1 <*> UI.userText entry2
        )
