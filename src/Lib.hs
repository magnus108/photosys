{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core

import           Item

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
    getBody window #+ []
