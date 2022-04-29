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

import           Monad
import           Env                            ( Env )
import qualified Env
import           Layout


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event User)
setup window = mdo

    -- GUI elements
    ((elemName, elemPassword, elemAdmin), tUser) <- liftUI $ dataItem bUser
    (createBtn                          , createBtnView) <- mkButton "Opret"

    -- GUI layout
    dataName <- mkInput "Name" (element elemName # set (attr "placeholder") "Fx Anders Andersen")
    dataPassword <- mkInput "Password" (element elemPassword # set UI.type_ "password")
    dataAdmin <- mkCheckbox "Admin" (element elemAdmin)



    closeBtn <- liftUI $ UI.button #. "modal-close is-large"
    modal    <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-content"
           #+ [UI.div #. "box" #+ [string "Opret godkendt"]]
           , element closeBtn
           ]

    elem <-
        liftUI
        $  UI.div
        #. "section is-medium"
        #+ [ UI.div
             #. "container"
             #+ [ element dataName
                , element dataPassword
                , element dataAdmin
                , element createBtnView
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
    liftUI $ element createBtn # sink UI.enabled bNotEmpty

    liftUI $ element modal # sink
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
