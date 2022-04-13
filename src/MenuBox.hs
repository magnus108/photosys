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


data ListBox a = ListBox
    { _elementLB   :: Element
    , _selectionLB :: Tidings (Maybe a)
    }

instance Widget (ListBox a) where
    getElement = _elementLB

userSelection :: ListBox a -> Tidings (Maybe a)
userSelection = _selectionLB

listBox
    :: forall a . Ord a => Behavior [a]
    -> Behavior (Maybe a)
    -> Behavior (a -> UI Element)
    -> UI (ListBox a)
listBox bitems bsel bdisplay = do
    list              <- UI.div #. "navbar-brand"

    (e :: Event a, h) <- liftIO $ newEvent

    let bDisplayButton =
            (\f x -> do
                    button <- UI.a #. "navbar-item" #+ [f x]
                    UI.on UI.click button $ \_ -> liftIO $ h x
                    return button
                )
                <$> bdisplay

    element list # sink items (map <$> bDisplayButton <*> bitems)

    -- animate output selection
        {-
    let bindices :: Behavior (Reexport.Map a Int)
        bindices = (Reexport.fromList . flip zip [0..]) <$> bitems
        bindex   = lookupIndex <$> bindices <*> bsel

        lookupIndex indices Nothing    = Nothing
        lookupIndex indices (Just sel) = Map.lookup sel indices

    element list # sink UI.selection bindex
    -}

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
        {-
    let bindices2 :: Behavior (Reexport.Map Int a)
        bindices2 = Reexport.fromList . zip [0..] <$> bitems

        _selectionLB = tidings bsel $
            lookupIndex <$> bindices2 <@> UI.selectionChange list
        _elementLB   = list
        -}


    let _selectionLB = tidings bsel (Just <$> e)
        _elementLB   = list

    return ListBox { .. }

items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
