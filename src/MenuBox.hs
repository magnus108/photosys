{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module MenuBox where

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



listBox
    :: forall a . Ord a => Behavior [a]
    -> Behavior (Maybe a)
    -> Behavior (a -> UI Element)
    -> UI (Behavior [UI Element], Tidings (Maybe a)) -- could be wrong
listBox bitems bsel bdisplay = do

    (e :: Event a, h) <- liftIO $ newEvent

    let bDisplayButton =
            (\f y x ->
                if (x `elem` y) then do
                        button <- UI.a #. "navbar-item is-active" #+ [f x]
                        UI.on UI.click button $ \_ -> liftIO $ h x
                        return button
                else do
                        button <- UI.a #. "navbar-item" #+ [f x]
                        UI.on UI.click button $ \_ -> liftIO $ h x
                        return button
                )
                <$> bdisplay <*> bsel

    let bElems = map <$> bDisplayButton <*> bitems
    let tiding = tidings bsel (Just <$> e)

    return (bElems, tiding)
