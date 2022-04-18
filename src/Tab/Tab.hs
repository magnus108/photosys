{-# LANGUAGE RecursiveDo #-}
module Tab.Tab where

import           Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )
import qualified MenuBox

import           Token                          ( Token )
import qualified Token
import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import           Item                           ( Item )
import qualified Item
import           Tab                            ( Tab )
import qualified Tab

import qualified Relude.Unsafe                 as Unsafe

import           Database

import qualified Data.List                     as List
import           Control.Bool


import           Monad
import           Env                            ( Env )
import qualified Env


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Tidings (Maybe DatabaseKey), Event Token)
setup window = mdo
    bDatabaseLoan        <- asks Env.bDatabaseLoan
    bDatabaseUser        <- asks Env.bDatabaseUser
    bDatabaseItem        <- asks Env.bDatabaseItem
    bDatabaseToken       <- asks Env.bDatabaseToken
    bSelectionToken      <- asks Env.bSelectionToken
    bDatabaseHistory     <- asks Env.bDatabaseHistory
    bDatabaseTab         <- asks Env.bDatabaseTab
    bSelectionTab        <- asks Env.bSelectionTab

    -- GUI elements
    (bListBox, tListBox) <- liftUI
        $ MenuBox.listBox bListBoxItems bSelectionTab bDisplayTab
    logoutBtn   <- liftUI $ UI.button #+ [string "Log ud"]

    -- GUI layout
    currentUser <-
        liftUI
        $  UI.div
        #. "navbar-item"
        #+ [UI.span #. "tag is-danger is-large" # sink text bSelectedUserName]
    list <-
        liftUI $ UI.div #. "navbar-brand" # sink (items currentUser) bListBox

    elem <-
        liftUI
        $  UI.mkElement "nav"
        #. "navbar is-primary is-spaced"
        #+ [ UI.div
             #. "container"
             #+ [ element list
                , UI.div
                #. "navbar-menu"
                #+ [ UI.div #. "navbar-start"
                   , UI.div
                   #. "navbar-end"
                   #+ [ UI.div
                        #. "navbar-item"
                        #+ [element logoutBtn #. "button"]
                      ]
                   ]
                ]
           ]

    -- Events and behaviors
    let eLogout = UI.click logoutBtn

    let bLookupTab :: Behavior (DatabaseKey -> Maybe Tab)
        bLookupTab = flip Database.lookup <$> bDatabaseTab

        bShowTab :: Behavior (DatabaseKey -> String)
        bShowTab = (maybe "" Tab.name .) <$> bLookupTab

        bShowTabAdmin :: Behavior (DatabaseKey -> Bool)
        bShowTabAdmin = (maybe False Tab.admin .) <$> bLookupTab

        bDisplayTab :: Behavior (DatabaseKey -> UI Element)
        bDisplayTab = (UI.string .) <$> bShowTab

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p q -> filter (\x -> q == p x) . keys)
                <$> bShowTabAdmin
                <*> bSelectedAdmin
                <*> bDatabaseTab

        bSelectionDataItem :: Behavior (Maybe Tab)
        bSelectionDataItem = (=<<) <$> bLookupTab <*> bSelectionTab

        bLookupUser :: Behavior (DatabaseKey -> Maybe User)
        bLookupUser = flip lookup <$> bDatabaseUser

        bShowUser :: Behavior (DatabaseKey -> String)
        bShowUser = (maybe "" User.name .) <$> bLookupUser

        bDisplayUserName :: Behavior (DatabaseKey -> UI Element)
        bDisplayUserName = (UI.string .) <$> bShowUser

        bSelectedUser :: Behavior (Maybe User)
        bSelectedUser = (=<<) <$> bLookupUser <*> bSelectedTokenId

        bSelectedUserName :: Behavior String
        bSelectedUserName = (maybe "" User.name) <$> bSelectedUser

        bSelectedAdmin :: Behavior Bool
        bSelectedAdmin = (maybe False User.admin) <$> bSelectedUser

        bLookupToken :: Behavior (DatabaseKey -> Maybe Token)
        bLookupToken = flip lookup <$> bDatabaseToken

        bSelectedToken :: Behavior (Maybe Token)
        bSelectedToken = (=<<) <$> bLookupToken <*> bSelectionToken

        bSelectedTokenId :: Behavior (Maybe Int)
        bSelectedTokenId = chainedTo Token.tokenId <$> bSelectedToken


    return (elem, tListBox, Token.NoToken <$ eLogout)

items user = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ ((element user) : i)
