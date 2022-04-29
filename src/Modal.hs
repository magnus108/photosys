{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Modal where

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
    { _elementModal  :: Element
    , _stateModal :: Tidings Bool
    }

instance Widget Modal where getElement = _elementModal

state :: Modal -> Tidings Bool
state = _stateModal


modal :: UI Element -> Behavior Bool -> UI Modal
modal elem bState = do
    closeBtn                   <- UI.button #. "modal-close is-large"
    _elementModal <- UI.div
        #+ [ UI.div #. "modal-background"
           , UI.div
           #. "modal-content"
           #+ [UI.div #. "box" #+ [elem]]
           , element closeBtn
           ]


    let _stateModal = tidings bState $ False <$ UI.click closeBtn

    element _elementModal # sink
        (attr "class")
        ((\b -> if b then "modal is-active" else "modal") <$> bState)

    return Modal {..}
