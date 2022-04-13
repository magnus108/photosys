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


setup
    :: Window
    -> Behavior (Database Loan)
    -> Behavior (Database User)
    -> Behavior (Database Item)
    -> UI (Element, Event Item)
setup window bDatabaseLoan bDatabaseUser bDatabaseItem = mdo

    -- GUI elements

    createBtn <- UI.button #+ [string "Opret"]
    ((elemName, elemCode, elemSerie, elemPrice, elemVendor), tItem) <- dataItem bItem

    -- GUI layout
    dataName  <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Name"]
           , UI.div
           #. "control"
           #+ [ element elemName #. "input" # set (attr "placeholder")
                                                  "Fx Kamera 1"
              ]
           ]

    dataCode <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Code"]
           , UI.div
           #. "control"
           #+ [ element elemCode #. "input" # set (attr "placeholder")
                                                  "Fx ABCDE"
              ]
           ]

    dataSerie <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Serie"]
           , UI.div
           #. "control"
           #+ [ element elemSerie #. "input" # set (attr "placeholder")
                                                  "Fx 13"
              ]
           ]

    dataPrice <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Pris"]
           , UI.div
           #. "control"
           #+ [ element elemPrice #. "input" # set (attr "placeholder")
                                                  "Fx 10.000"
              ]
           ]

    dataVendor <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Forhandler"]
           , UI.div
           #. "control"
           #+ [ element elemVendor #. "input" # set (attr "placeholder")
                                                  "Fx Kamera shoppen"
              ]
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
                , element dataCode
                , element dataSerie
                , element dataPrice
                , element dataVendor
                , element createBtn'
                , element modal
                ]
           ]


    -- Events and behaviors
    let eCreate = UI.click createBtn
        eClose  = UI.click closeBtn
        eItemIn = rumors tItem

    bActiveModal <- stepper False $ Unsafe.head <$> unions
        [True <$ eCreate, False <$ eClose]


    bItem <- stepper Nothing $ Unsafe.head <$> unions
        [ Just <$> eItemIn
        , Just emptyItem <$ eCreate
        ]

    let bNotEmpty = isJust <$> bItem
    element createBtn # sink UI.enabled bNotEmpty

    element modal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bActiveModal)

    return (elem, filterJust $ bItem <@ eCreate)


emptyItem :: Item
emptyItem = Item.Item "" "" "" "" ""

dataItem :: Behavior (Maybe Item) -> UI ((Element, Element, Element, Element, Element), Tidings Item)
dataItem bItem = do
    entry1 <- UI.entry $ Item.name . fromMaybe emptyItem <$> bItem
    entry2 <- UI.entry $ Item.code . fromMaybe emptyItem <$> bItem
    entry3 <- UI.entry $ Item.serie . fromMaybe emptyItem <$> bItem
    entry4 <- UI.entry $ Item.price . fromMaybe emptyItem <$> bItem
    entry5 <- UI.entry $ Item.vendor . fromMaybe emptyItem <$> bItem

    return
        ( (getElement entry1, getElement entry2, getElement entry3, getElement entry4, getElement entry5)
        , Item.Item <$> UI.userText entry1 <*> UI.userText entry2 <*> UI.userText entry3 <*> UI.userText entry4 <*>  UI.userText entry5
        )
