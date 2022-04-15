{-# LANGUAGE RecursiveDo #-}
module Token.Create where

import qualified Data.Text as T
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

setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> Behavior (Database Token)
    -> Behavior (Maybe DatabaseKey)
    -> UI (Element, Event Token)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem bDatabaseToken bSelectionToken
    = mdo
        -- GUI elements
        ((elemName, elemPassword), tLogin) <- dataItem bLogin
        createBtn                          <- UI.button #+ [string "Login"]

        -- GUI layout
        dataName                           <-
            UI.div
            #. "field"
            #+ [ UI.label #. "label" #+ [string "Username"]
               , UI.div #. "control" #+ [element elemName #. "input"]
               ]

        dataPassword <-
            UI.div
            #. "field"
            #+ [ UI.label #. "label" #+ [string "Password"]
               , UI.div
               #. "control"
               #+ [element elemPassword #. "input" # set UI.type_ "password"]
               ]

        createBtn' <-
            UI.div
            #. "field"
            #+ [UI.div #. "control" #+ [element createBtn #. "button"]]

        elem <-
            UI.div
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
                    && (checkPassword (mkPassword $ T.pack $ Login.password y) (PasswordHash (T.pack $ User.password x) :: PasswordHash Bcrypt) == PasswordCheckSuccess)

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

        element createBtn
            # sink UI.enabled (bDisplayItem <&&> (not <$> bHasToken))

        return (elem, maybe Token.NoToken Token.Token <$> bUser <@ eCreate)


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
