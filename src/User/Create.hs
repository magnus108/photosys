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

import           Behaviors

import qualified Modal

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event User)
setup window = mdo

    ((elemName, elemPassword, elemAdmin), tUser) <- liftUI $ dataItem bUser
    (createBtn                          , createBtnView) <- mkButton "Opret"

    dataName <- mkInput
        "Name"
        (element elemName # set (attr "placeholder") "Fx Anders Andersen")
    dataPassword <- mkInput "Password"
                            (element elemPassword # set UI.type_ "password")
    dataAdmin <- mkCheckbox "Admin" (element elemAdmin)

    (modalView, modal) <- mkModal bActiveModal [UI.string "Opret godkendt"]

    elem      <- mkContainer
        [ element dataName
        , element dataPassword
        , element dataAdmin
        , element createBtnView
        , element modalView
        ]

    -- Events and behaviors
    let eCreate = UI.click createBtn
        eModal  = rumors $ Modal.state modal
        eUserIn = rumors tUser

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, eModal]

    bUser <- stepper Nothing $ Unsafe.head <$> unions
        [(\x -> if x == emptyUser then Nothing else Just x) <$> eUserIn, Nothing <$ eCreate]


    bDatabaseUser   <- asks Env.bDatabaseUser
    bLookupUser <- lookupUser

    let bUsers :: Behavior [String]
        bUsers =  (\db lookup -> fmap User.name $ catMaybes $ fmap lookup $ keys db)
                <$> bDatabaseUser <*> bLookupUser

    let isError = (\x xs -> List.notElem x (fmap Just xs)) <$> (fmap User.name <$> bUser) <*> bUsers
    let bNotEmpty = isJust <$> bUser
    liftUI $ element createBtn # sink UI.enabled (bNotEmpty <&&> isError)
    liftUI $ element elemName # sink UI.class_ ((\b x -> if b && x then "input" else "input is-danger") <$> isError <*> bNotEmpty )

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
