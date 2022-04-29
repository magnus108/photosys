module Layout where

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import           Reactive.Threepenny
import           Monad
import           Env


mkButton
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => String
    -> m (Element, Element)
mkButton title = liftUI $ do
    button <- UI.button #+ [string title]
    view   <-
        UI.div
        #. "field"
        #+ [UI.div #. "control" #+ [element button #. "button"]]
    return (button, view)

mkSearch
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Behavior String
    -> m (UI.TextEntry, Element)
mkSearch bFilterItem = liftUI $ do
    filter <- liftUI $ UI.entry bFilterItem
    view   <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string "Søg"]
           , UI.div
           #. "control"
           #+ [element filter #. "input" # set (attr "placeholder") "Fx Kamera"]
           ]
    return (filter, view)


mkListBox
    :: forall m a
     . (Ord a, MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Behavior [a]
    -> Behavior (Maybe a)
    -> Behavior (a -> UI Element)
    -> m (UI.ListBox a, Element)
mkListBox bItems bSel bDisplay = liftUI $ do
    listBox <- UI.listBox bItems bSel bDisplay
    view    <-
        UI.div
        #. "field"
        #+ [ UI.div
             #. "control is-expanded"
             #+ [ UI.div
                  #. "select is-multiple is-fullwidth"
                  #+ [ element listBox # set (attr "size") "5" # set
                           (attr "multiple")
                           ""
                     ]
                ]
           ]
    return (listBox, view)

