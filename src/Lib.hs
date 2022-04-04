{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core

import           System.FilePath

import           Item

someFunc :: Int -> String -> IO ()
someFunc port root = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic = Just (root </> "static")
                           , jsCustomHTML               = Just "index.html"
                           }
        $ setup


setup :: Window -> UI ()
setup window = void $ mdo
    return window # set title "PhotoApp"
    getBody window #+ []
