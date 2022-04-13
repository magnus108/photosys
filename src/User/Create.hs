{-# LANGUAGE RecursiveDo #-}
module User.Create where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Loan                           ( Loan )
import qualified Loan
import           User                           ( User )
import qualified User
import           Item                           ( Item )
import qualified Item

import qualified Relude.Unsafe                 as Unsafe

import           Database

import qualified Data.List                     as List
import           Control.Bool
import qualified Checkbox


setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> UI (Element, Event User)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem = mdo

    -- GUI elements
    ((elemName, elemPassword, elemAdmin), tUser) <- dataItem bUser
    createBtn <- UI.button #+ [string "Opret"]

    -- GUI layout
    dataName  <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Name"]
           , UI.div
           #. "control"
           #+ [ element elemName #. "input" # set (attr "placeholder")
                                                  "Fx Anders Andersen"
              ]
           ]

    dataPassword <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Password"]
           , UI.div
           #. "control"
           #+ [element elemPassword #. "input" # set UI.type_ "password"]
           ]

    dataAdmin <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Admin"]
           , UI.div #. "control" #+ [element elemAdmin #. "checkbox"]
           ]

    createBtn' <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element createBtn #. "button"]]


    closeBtn <- UI.button #. "modal-close is-large"
    modal    <-
        UI.div
            #+ [ UI.div #. "modal-background"
               , UI.div
               #. "modal-content"
               #+ [UI.div #. "box" #+ [string "Opret godkendt"]]
               , element closeBtn
               ]

    elem <-
        UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element dataName
                , element dataPassword
                , element dataAdmin
                , element createBtn'
                , element modal
                ]
           ]


    -- Events and behaviors
    let eCreate = UI.click createBtn
        eClose  = UI.click closeBtn
        eUserIn = rumors tUser

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]


    bUser <- stepper Nothing $ Unsafe.head <$> unions
        [Just <$> eUserIn, Just emptyUser <$ eCreate]

    let bNotEmpty = isJust <$> bUser
    element createBtn # sink UI.enabled bNotEmpty

    element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    return (elem, filterJust $ bUser <@ eCreate)


emptyUser :: User
emptyUser = User.User "" "" False

dataItem
    :: Behavior (Maybe User) -> UI ((Element, Element, Element), Tidings User)
dataItem bUser = do
    entry1 <- UI.entry $ User.name . fromMaybe emptyUser <$> bUser
    entry2 <- UI.entry $ User.password . fromMaybe emptyUser <$> bUser
    entry3 <- Checkbox.entry $ User.admin . fromMaybe emptyUser <$> bUser

    return
        ( (getElement entry1, getElement entry2, getElement entry3)
        , User.User
        <$> UI.userText entry1
        <*> UI.userText entry2
        <*> Checkbox.userCheck entry3
        )
