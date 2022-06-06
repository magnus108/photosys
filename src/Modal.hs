{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Modal where

import qualified Relude.Unsafe                 as Unsafe
import           Control.Monad                  ( void
                                                , when
                                                )

import qualified Relude.Container.Reexport     as Reexport
import qualified Relude.Extra.Map              as Map
import qualified Graphics.UI.Threepenny.Attributes
                                               as UI
import qualified Graphics.UI.Threepenny.Events as UI
import qualified Graphics.UI.Threepenny.Elements
                                               as UI
import qualified Graphics.UI.Threepenny.Core   as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny


data Modal = Modal
    { _elementCloseBtn :: Element
    , _stateModal :: Tidings Bool
    }


state :: Modal -> Tidings Bool
state = _stateModal


modal :: Behavior Bool -> UI Modal
modal bState = do
    _elementCloseBtn <- UI.input
    let _stateModal = tidings bState $ Unsafe.head <$> unions [False <$ UI.click _elementCloseBtn]
    return Modal {..}
