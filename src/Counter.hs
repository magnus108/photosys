{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Counter where

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



counter :: forall a . Ord a => Behavior [a] -> UI Element
counter bitems = do
    content <- UI.span
    let bCount = (show . length) <$> bitems
    element content #. "tag is-info is-large" # sink text bCount
    -- overvej flyt styling ud
