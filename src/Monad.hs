module Monad
    ( App(..)
    , runApp
    , MyApp
    )
where

import           Graphics.UI.Threepenny.Core
import           Env


newtype App env a = App
    { unApp :: ReaderT env UI a
    } deriving newtype ( Functor, Applicative, Monad, MonadReader env, MonadIO, MonadUI, MonadFix)

instance (MonadUI m) => MonadUI (ReaderT r m) where
    liftUI = lift . liftUI

type MyApp = App Env

runApp :: env -> App env a -> UI a
runApp env = usingReaderT env . unApp
{-# INLINE runApp #-}
