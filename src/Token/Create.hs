{-# LANGUAGE RecursiveDo #-}
module Token.Create where

import           Data.Time
import qualified Data.Text                     as T
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import           Item                           ( Item )
import qualified Item
import           Token                          ( Token )
import qualified Token
import           Login                          ( Login )
import qualified Login

import qualified Relude.Unsafe                 as Unsafe

import           Database

import qualified Data.List                     as List
import           Control.Bool
import           Data.Password.Bcrypt

import           Monad
import           Env                            ( Env )
import qualified Env


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Token)
setup window = mdo
    bDatabaseLoan                      <- asks Env.bDatabaseLoan
    bDatabaseUser                      <- asks Env.bDatabaseUser
    bDatabaseItem                      <- asks Env.bDatabaseItem
    bDatabaseToken                     <- asks Env.bDatabaseToken
    bSelectionToken                    <- asks Env.bSelectionToken
    bDatabaseHistory                   <- asks Env.bDatabaseHistory

    -- GUI elements
    ((elemName, elemPassword), tLogin) <- liftUI $ dataItem bLogin
    createBtn                          <- liftUI $ UI.button #+ [string "Login"]

    -- GUI layout
    dataName                           <-
        liftUI
        $  UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Username"]
           , UI.div #. "control" #+ [element elemName #. "input"]
           ]

    dataPassword <-
        liftUI
        $  UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Password"]
           , UI.div
           #. "control"
           #+ [element elemPassword #. "input" # set UI.type_ "password"]
           ]

    createBtn' <-
        liftUI
        $  UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]

    elem <-
        liftUI
        $  UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [element dataName, element dataPassword, element createBtn']
           ]

    let eCreate  = UI.click createBtn
        eLoginIn = rumors tLogin

    bLogin <- stepper Nothing $ Unsafe.head <$> unions
        [Just <$> eLoginIn, Just emptyLogin <$ eCreate]


    let compareLogin :: Login -> User -> Bool
        compareLogin y x =
            (User.name x == Login.name y)
                && (  checkPassword
                           (mkPassword $ T.pack $ Login.password y)
                           (PasswordHash (T.pack $ User.password x) :: PasswordHash
                                 Bcrypt
                           )
                   == PasswordCheckSuccess
                   )

        bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken


        bUser :: Behavior (Maybe DatabaseKey)
        bUser =
            (\p q -> find (and . liftA2 compareLogin p . q) . keys)
                <$> bLogin
                <*> bLookupUser
                <*> bDatabaseUser

    let bDisplayItem :: Behavior Bool
        bDisplayItem = isJust <$> bLogin

        bHasToken :: Behavior Bool
        bHasToken = maybe False Token.isToken <$> bSelectedToken

    liftUI $ element createBtn # sink
        UI.enabled
        (bDisplayItem <&&> (not <$> bHasToken))

---------
    timer <- liftUI $ UI.timer # set UI.interval 1000
    let eTick = UI.tick timer

    (eTime, hTime) <- liftIO $ newEvent

    c              <- liftIO $ getCurrentTime

    bTimer         <- stepper (show c) $ Unsafe.head <$> unions [eTime]

    liftUI $ onEvent eTick $ \items -> do
        c <- liftIO $ getCurrentTime
        liftIO $ hTime (show c)

    liftUI $ UI.start timer
---------

    return (elem, maybe (const Token.NoToken) Token.Token <$> bUser <*> bTimer <@ eCreate)


emptyLogin :: Login
emptyLogin = Login.Login "" ""


dataItem :: Behavior (Maybe Login) -> UI ((Element, Element), Tidings Login)
dataItem bItem = do
    entry1 <- UI.entry $ Login.name . fromMaybe emptyLogin <$> bItem
    entry2 <- UI.entry $ Login.password . fromMaybe emptyLogin <$> bItem

    return
        ( (getElement entry1, getElement entry2)
        , Login.Login <$> UI.userText entry1 <*> UI.userText entry2
        )
