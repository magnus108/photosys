{-# LANGUAGE RecursiveDo #-}
module Item.Create where

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

import           Monad
import           Env                            ( Env )
import qualified Env
import           Layout
import           Behaviors


setup
    :: (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Window
    -> m (Element, Event Item,  Event (Database Item -> Database Item))
setup window = mdo

    (filterItem , searchItem  ) <- mkSearch bFilterEntryItem
    (listBoxItem, dropdownItem) <- mkListBox bListBoxItems
                                             bSelectionItem
                                             bDisplayItemName
    counter                    <- mkCounter bListBoxItems

    -- GUI elements
    (changeBtn, changeBtnView) <- mkButton "Ændre"
    (createBtn, createBtnView) <- mkButton "Opret"
    ((elemName, elemCode, elemSerie, elemPrice, elemVendor, elemInvoiceNumber, elemDateOfPurchase, elemNote), tItem) <-
        liftUI $ dataItem bItem

    -- GUI layout
    dataName <- mkInput
        "Name"
        (element elemName # set (attr "placeholder") "Fx Kamera 1")

    dataCode <- mkInput
        "Stregkode"
        (element elemCode # set (attr "placeholder") "Fx ABCDE")
    dataSerie <- mkInput
        "S/N"
        (element elemSerie # set (attr "placeholder") "Fx 13")

    dataPrice <- mkInput
        "Pris"
        (element elemPrice # set (attr "placeholder") "Fx 10.000")
    dataVendor <- mkInput
        "Forhandler"
        (element elemVendor # set (attr "placeholder") "Fx Kamera shoppen")
    dataInvoiceNumber  <- mkInput "Fakture nr." (element elemInvoiceNumber)

    dataDateOfPurchase <- mkInput "Købsdato" (element elemDateOfPurchase)

    dataNote           <- mkInput "Note" (element elemNote)


    closeBtn                   <- liftUI $ UI.input # set UI.type_ "button" #. "button" # set value "Luk"
    modal                      <-
        liftUI
        $  UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-card"
           #+ [ UI.mkElement "section"
              #. "modal-card-body"
              #+ [string "Opret godkendt"]
              , UI.mkElement "footer" #. "modal-card-foot" #+ [element closeBtn]
              ]
           ]

    elem <- mkContainer
        [ element dataName
        , element dataCode
        , element dataSerie
        , element dataPrice
        , element dataVendor
        , element dataInvoiceNumber
        , element dataDateOfPurchase
        , element dataNote
        , element createBtnView
        , element changeBtnView
        , element searchItem
        , element dropdownItem
        , element counter
        , element modal
        ]


    -- Events and behaviors
    let eCreate = UI.click createBtn
        eChange = UI.click changeBtn
        eClose  = UI.click closeBtn
        eItemIn = rumors tItem
    let eSelectionItem = rumors $ UI.userSelection listBoxItem

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose, True <$ eChange]


    bItem <- stepper Nothing $ Unsafe.head <$> unions
        [Just <$> eItemIn, Just emptyItem <$ eCreate , bLookupItem <@> (filterJust $ eSelectionItem )]

    bFilterEntryItem <- stepper "" . rumors $ UI.userText filterItem


    let isInfixOf :: (Eq a) => [a] -> [a] -> Bool
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    let tFilterItem = isInfixOf <$> UI.userText filterItem
        bFilterItem = facts tFilterItem
        eFilterItem = rumors tFilterItem


    bSelectionItem <- stepper Nothing $ Unsafe.head <$> unions
        [ eSelectionItem
        , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
        <$> bSelectionItem
        <*> bShowDataItem
        <@> eFilterItem
        ]



    bDatabaseLoan    <- asks Env.bDatabaseLoan
    bDatabaseUser    <- asks Env.bDatabaseUser
    bDatabaseItem    <- asks Env.bDatabaseItem
    bDatabaseToken   <- asks Env.bDatabaseToken
    bSelectionToken  <- asks Env.bSelectionToken
    bDatabaseHistory <- asks Env.bDatabaseHistory

    bLookupItem     <- lookupItem

    let bNotEmpty = isJust <$> bItem
    let bShowItemCode :: Behavior (DatabaseKey -> String)
        bShowItemCode = (maybe "" Item.code .) <$> bLookupItem
        bShowDataItem :: Behavior (DatabaseKey -> String)
        bShowDataItem = (maybe "" Item.showItem .) <$> bLookupItem

        bDisplayItemName :: Behavior (DatabaseKey -> UI Element)
        bDisplayItemName = (UI.string .) <$> bShowDataItem


        bCode = fmap Item.code <$> bItem

        bListBoxItems :: Behavior [DatabaseKey]
        bListBoxItems =
            (\p show ->
                    filter (p . show) . keys
                )
                <$> bFilterItem
                <*> bShowDataItem
                <*> bDatabaseItem


        bItems :: Behavior [String]
        bItems =  (\db lookup -> fmap Item.code $ catMaybes $ fmap lookup $ keys db)
                <$> bDatabaseItem <*> bLookupItem
        isError = (\x xs -> List.notElem x (fmap Just xs)) <$> bCode <*> bItems


    liftUI $ element elemCode # sink UI.class_ ((\b x -> if b || (isJust x) then "input" else "input is-danger") <$> isError <*> bSelectionItem)
    liftUI $ element createBtn # sink UI.enabled (bNotEmpty <&&> isError)
    liftUI $ element changeBtn # sink UI.enabled (bNotEmpty <&&> (isJust <$> bSelectionItem))
    liftUI $ element elemCode # sink UI.enabled (isNothing <$> bSelectionItem)

    liftUI $ element modal # sink (modalSink closeBtn) bActiveModal

    return (elem, filterJust $ bItem <@ eCreate,filterJust $ update' <$> bSelectionItem <@> (filterJust $ bItem <@ eChange))


emptyItem :: Item
emptyItem = Item.Item "" "" "" "" "" "" "" ""

dataItem
    :: Behavior (Maybe Item)
    -> UI
           ( ( Element
             , Element
             , Element
             , Element
             , Element
             , Element
             , Element
             , Element
             )
           , Tidings Item
           )
dataItem bItem = do
    entry1 <- UI.entry $ Item.name . fromMaybe emptyItem <$> bItem
    entry2 <- UI.entry $ Item.code . fromMaybe emptyItem <$> bItem
    entry3 <- UI.entry $ Item.serie . fromMaybe emptyItem <$> bItem
    entry4 <- UI.entry $ Item.price . fromMaybe emptyItem <$> bItem
    entry5 <- UI.entry $ Item.vendor . fromMaybe emptyItem <$> bItem
    entry6 <- UI.entry $ Item.invoiceNumber . fromMaybe emptyItem <$> bItem
    entry7 <- UI.entry $ Item.dateOfPurchase . fromMaybe emptyItem <$> bItem
    entry8 <- UI.entry $ Item.note . fromMaybe emptyItem <$> bItem

    return
        ( ( getElement entry1
          , getElement entry2
          , getElement entry3
          , getElement entry4
          , getElement entry5
          , getElement entry6
          , getElement entry7
          , getElement entry8
          )
        , Item.Item
        <$> UI.userText entry1
        <*> UI.userText entry2
        <*> UI.userText entry3
        <*> UI.userText entry4
        <*> UI.userText entry5
        <*> UI.userText entry6
        <*> UI.userText entry7
        <*> UI.userText entry8
        )

