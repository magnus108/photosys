{-# LANGUAGE RecursiveDo #-}
module Lib
    ( someFunc
    )
where

import Data.Aeson

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
                                         hiding ( delete )

import           Item

import qualified Relude.Unsafe                 as Unsafe
import Database


import qualified UserGui
import qualified LoanGui
import qualified ItemGui
import qualified LoginGui

import qualified Data.ByteString.Lazy as BS


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
    LoginGui.setup window
    ItemGui.setup window
    UserGui.setup window
    LoanGui.setup window
