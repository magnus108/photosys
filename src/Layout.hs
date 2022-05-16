module Layout where

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import           Reactive.Threepenny
import           Monad
import           Env
import qualified Counter


mkCounter
    :: forall m a
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Behavior [a]
    -> m Element
mkCounter xs = liftUI $ do
    count <- Counter.counter xs
    view <- element count #. "tag is-info is-large"
    return view


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


mkCheckbox
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => String
    -> UI Element
    -> m Element
mkCheckbox label elem =
    liftUI
        $  UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string label]
           , UI.div #. "control" #+ [elem #. "checkbox"]
           ]


mkSearch
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => Behavior String
    -> m (UI.TextEntry, Element)
mkSearch bFilterItem = do
    filter <- liftUI $ UI.entry bFilterItem
    view   <- mkInput "Søg" $ element filter
    return (filter, view)

mkInput
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => String
    -> UI Element
    -> m Element
mkInput label elem = liftUI $ do
    view <-
        UI.div
        #. "field"
        #+ [ UI.label #. "label" #+ [string label]
           , UI.div #. "control" #+ [elem #. "input"]
           ]
    return view


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


mkContainer
    :: forall m
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m)
    => [UI Element]
    -> m Element
mkContainer elems =
    liftUI $ UI.div #. "section is-medium" #+ [UI.div #. "container" #+ elems]



mkSearchEntry
    :: forall m a
     . (MonadReader Env m, MonadUI m, MonadIO m, MonadFix m, Ord a)
    => Behavior [a]
    -> Behavior (Maybe a)
    -> Behavior (a -> UI Element)
    -> Behavior String
    -> m (Element,Element,Element, UI.TextEntry, UI.ListBox a)
mkSearchEntry bItems bSel bDisplay bFilterItem = do
    (filter, filterView) <- mkSearch bFilterItem
    (listBox, listBoxView) <- mkListBox bItems bSel bDisplay
    counterView                 <- mkCounter bItems
    return (filterView, listBoxView,  counterView, filter, listBox)
