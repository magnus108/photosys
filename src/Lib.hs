{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core

import           Item

import qualified Relude.Unsafe                 as Unsafe

someFunc :: Int -> IO ()
someFunc port = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just "static"
                           , jsCustomHTML               = Just "index.html"
                           }
        $ setup


setup :: Window -> UI ()
setup window = void $ mdo
    return window # set title "PhotoApp"

    entryName   <- UI.entry $ name <$> bItem
    entryCode   <- UI.entry $ code <$> bItem
    filterEntry <- UI.entry bFilterString

    createBtn   <- UI.button #+ [string "Create"]
    let eCreate = UI.click createBtn

    let items = grid [[string "Name:", element entryName #. "input", string "Code:", element entryCode #. "input"]]
    listBox <- UI.listBox bItems bSelection bDisplayItem

    getBody window
        #+ [ UI.div
             #. "container"
             #+ [ grid
                      [ [row [element filterEntry #. "input"]]
                      , [element listBox, items]
                      , [row [element createBtn #. "button"]]
                      ]
                ]
           ]

    bFilterString <- stepper "" . rumors $ UI.userText filterEntry

    bItem         <- stepper (Item "bob" "142A23") $ Unsafe.head <$> unions []

    bSelection    <- stepper Nothing $ Unsafe.head <$> unions []

    bItems        <- accumB [] $ concatenate <$> unions
        [create (Item "Emil" "1423123") <$ eCreate]

    let bDisplayItem = pure (UI.string . name)

    onChanges bItems $ \items -> do
        liftIO $ putStrLn (show items)


create :: a -> [a] -> [a]
create = (:)
