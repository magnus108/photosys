{-# LANGUAGE RecursiveDo #-}
module Loan.Delete where

import           Utils.Utils
import           Data.Aeson

import Data.Functor.Contravariant.Divisible
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import qualified Counter
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
import           Monad
import           Env                            ( Env )
import qualified Env
import           Layout
import           Behaviors
import           Loan.Behaviors



data DeleteEntry = DeleteEntry
    { _elementDE :: Element
    , _eDeleteLoan :: Event DatabaseKey
    , _userFilterDE    :: Tidings String
    , _itemFilterDE :: Tidings String
    , _userSelectionDE :: Tidings (Maybe DatabaseKey)
    , _itemSelectionDE :: Tidings (Maybe DatabaseKey)
    }

instance Widget DeleteEntry where getElement = _elementDE

setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m DeleteEntry
setup window = mdo

    -- GUI elements
    (filterUser , searchUser  ) <- mkSearch bFilterEntryUser
    (listBoxUser, dropdownUser) <- mkListBox bListBoxUsers
                                             bSelectionUser
                                             bDisplayUserName
    counterUser                 <- mkCounter bListBoxUsers

    (filterItem , searchItem  ) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems
                                             bSelectionItem
                                             bDisplayItem
    counterItem                <- mkCounter bListBoxItems


    loanInfo                   <- liftUI $ UI.span
    (deleteBtn, deleteBtnView) <- mkButton "Aflever"

    -- GUI layout
    closeBtn <- liftUI $ UI.input # set UI.type_ "button" #. "button" # set
        value
        "Luk"

    modal <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-card"
           #+ [ UI.mkElement "section"
              #. "modal-card-body"
              #+ [string "Aflevering godkendt: ", element loanInfo]
              , UI.mkElement "footer" #. "modal-card-foot" #+ [element closeBtn]
              ]
           ]

    _elementDE <- mkContainer
        [ element searchUser
        , element dropdownUser
        , element counterUser
        , element searchItem
        , element dropdownItem
        , element deleteBtnView
        , element counterItem
        , element modal
        ]


    -- Events and behaviors
    let eDelete = UI.click deleteBtn
    let eClose         = Unsafe.head <$> unions [UI.click closeBtn, () <$ UI.keypress closeBtn]

    bFilterEntryUser <- asks Env.bDeleteLoanFilterUser
    bFilterEntryItem <- asks Env.bDeleteLoanFilterItem


    let tFilterUser = isInfixOf <$> UI.userText filterUser
        bFilterUser = facts tFilterUser
        eFilterUser = rumors tFilterUser

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem

    let eSelectionUser = rumors $ UI.userSelection listBoxUser
        eSelectionItem = rumors $ UI.userSelection listBoxItem


    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eDelete, False <$ eClose]



    bSelectionUser  <- asks Env.bDeleteLoanSelectionUser
    bSelectionItem  <- asks Env.bDeleteLoanSelectionItem
    bDatabaseLoan   <- asks Env.bDatabaseLoan
    bDatabaseUser   <- asks Env.bDatabaseUser
    bDatabaseItem   <- asks Env.bDatabaseItem
    bDatabaseToken  <- asks Env.bDatabaseToken
    bSelectionToken <- asks Env.bSelectionToken


    bLookupUser     <- lookupUser
    bLookupItem     <- lookupItem
    bLookupLoan     <- lookupLoan

    bLoanItemId <- loanItemId
    bLoanUserId <- loanUserId

    bShowUser <- showUser

    bSelectedUser <- selectedUserDelete
    bSelectedItem <- selectedItemDelete

    bDisplayUserName <- displayUser
    bShowItem <- showItem
    bDisplayItem <- displayItem
    bDisplayItemSelected <- displayItemDelete

    let bAllLoans = keys <$> bDatabaseLoan


-------------------------------------------------------------------------------
    let what = Predicate (const False)
    let what1 = Predicate . flip (maybe True . (==))
    let what2 x y = divide (\l -> (Loan.item l, Loan.user l)) (what1 x) (what1 y)
    let what3 x y = choose (maybeToRight ()) what (what2 x y)
    let what4 f x y = contramap f $ what3 x y
    let what5 = what4 <$> bLookupLoan <*> bSelectionItem <*> bSelectionUser


    let what6 = filter . getPredicate <$> what5
    let what7 = what6 <*> bAllLoans
-------------------------------------------------------------------------------
    let sod = Predicate (const False)
    let sod1 = Predicate <$> bFilterUser
    let sod2 = Predicate <$> bFilterItem
    let sod5 = contramap <$> bShowUser <*> sod1
    let sod6 = contramap <$> bShowItem <*> sod2
    let sod7 = divide (\l -> (Loan.item l, Loan.user l)) <$> sod5 <*> sod6
    let sod8 = choose (maybeToRight ()) sod <$> sod7
    let sod9 = contramap <$> bLookupLoan <*> sod8
    let sod10 = filter . getPredicate <$> sod9
    let sod11 = sod10 <*> bAllLoans

    let bloansf = (filter . getPredicate . mconcat) <$> (sequenceA [sod9, what5])

-------------------------------------------------------------------------------
    let sode1 = Predicate <$> eFilterUser
    let sode3 = contramap <$> bShowUser <@> sode1
    let sode7 = divide (\l -> (Loan.item l, Loan.user l)) <$> sod6 <@> sode3
    let sode8 = choose (maybeToRight ()) sod <$> sode7
    let sode9 = contramap <$> bLookupLoan <@> sode8
    let soder = divide (\x -> (x, x)) <$> (pure sod) <@> sode9
    let sode10 = flip (filter . getPredicate) <$> bAllLoans <@> soder

    {-
    let sode1 x = Predicate x
    let sode3 x = flip contramap (sode1 x) <$> bShowUser
    let sode7 x y = divide (\l -> (Loan.item l, Loan.user l)) <$> sode3 x <*> sode3 y
    let sode8 x y = choose (maybeToRight ()) sod <$> (sode7 x y)
    let sode9 x y = contramap <$> bLookupLoan <*> (sod8 x y)
    let sode10 x y = filter . getPredicate <$> (sode9 x y)
    let sode11 = sode10 <$> bFilterUser
    -}
    --let sode12 = sode11 <$> bFilterUser <@> eFilterUser
    
    --let sode7 = divide (\l -> (Loan.user l, Loan.user l)) <$> sode3
    _

-------------------------------------------------------------------------------

    let bLoans' = (\f x y p s q r ->
                            filter (\l -> y == (Loan.item <$> (f l)) || y == Nothing)
                          . filter (\l -> x == (Loan.user <$> (f l)) || x == Nothing)
                          . filter (\l -> and $ (p . s . Loan.user) <$> (f l))
                          . filter (\l -> and $ (q . r . Loan.item) <$> (f l))
                          ) <$> bLookupLoan <*> bSelectionUser <*> bSelectionItem <*> bFilterUser <*> bShowUser <*> bFilterItem <*> bShowItem

    let bSearchLoans = bLoans' <*> bAllLoans

    let bListBoxUsers' = (\f xs -> fmap Loan.user $ catMaybes $ fmap f xs) <$> bLookupLoan <*> bSearchLoans
    let bListBoxUsers = (\xs -> filter (\x -> List.elem x xs) . keys) <$> bListBoxUsers' <*> bDatabaseUser
    let bListBoxItems' = (\f xs -> fmap Loan.item $ catMaybes $ fmap f xs)<$> bLookupLoan <*> bSearchLoans
    let bListBoxItems = (\xs -> filter (\x -> List.elem x xs) . keys) <$> bListBoxItems' <*> bDatabaseItem

    let bSelectedLoan = listToFirst <$> bSearchLoans
        hasSelectedLoan = isJust <$> bSelectedLoan


    liftUI $ element loanInfo # sink items bDisplayItemSelected
    liftUI $ element deleteBtn # sink UI.enabled hasSelectedLoan
    liftUI $ element modal # sink (modalSink closeBtn) bActiveModal

    let _userSelectionDE = tidings bSelectionUser $ Unsafe.head <$> unions
            [ eSelectionUser
            , (\ax f x y s q r p -> 
                            let gg = filter (\l -> y == (Loan.item <$> (f l)) || y == Nothing) $ filter (\l -> and $ (p . s . Loan.user) <$> (f l)) $ filter (\l -> and $ (q . r . Loan.item) <$> (f l)) ax
                                  in case listToFirst $ fmap Loan.user $ catMaybes $ fmap f gg of
                                        Just x -> Just x
                                        Nothing -> Nothing
                          ) <$> bAllLoans <*> bLookupLoan <*> bSelectionUser <*> bSelectionItem <*> bShowUser <*> bFilterItem <*> bShowItem <@> eFilterUser
            , Nothing <$ eClose
            ]

        _itemSelectionDE = tidings bSelectionItem $ Unsafe.head <$> unions
            [ eSelectionItem
            , (\b s items p -> case filter (p . s) (keys items) of
                  (x : []) -> Just x
                  (xs    ) -> b >>= \a -> if p (s a) then Just a else Nothing
              )
            <$> bSelectionItem
            <*> bShowItem
            <*> bDatabaseItem
            <@> eFilterItem
            , Nothing <$ eClose
            ]

        _userFilterDE =
            tidings bFilterEntryUser $ Unsafe.head <$> unions
                [rumors $ UI.userText filterUser, "" <$ eClose]

        _itemFilterDE = tidings bFilterEntryItem $ Unsafe.head <$> unions
            [rumors $ UI.userText filterItem, "" <$ eClose]

        _eDeleteLoan = filterJust $ bSelectedLoan <@ eClose

    return DeleteEntry { .. }

modalSink e = mkWriteAttr $ \b x -> void $ do
    return x # set (attr "class") (if b then "modal is-active" else "modal")
    if b then UI.setFocus e else return ()


listToFirst :: [a] -> Maybe a
listToFirst (x:[]) = Just x
listToFirst _ = Nothing


items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ [i]
