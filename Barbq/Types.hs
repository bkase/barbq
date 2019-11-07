{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, KindSignatures, DefaultSignatures, GADTs, DeriveFunctor, TemplateHaskell #-}

module Barbq.Types (
  Environment(..),
  M(..),
  MonadIntervalRunner(..),
  MonadShell(..),
  Task(..),
  ProviderAtom(..), providerConf, providerDefault, providerTask,
  Provider(..)) where

import Relude
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Lens (makeLenses)
import Control.Concurrent.Async.Timer (TimerConf)
import Control.Applicative.Free as A
import Pipes.Concurrent (Input, Output)

-- Base Monad
newtype Environment = Environment Int

newtype M a = M (ReaderT Environment IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadMask, MonadCatch, MonadThrow)

-- Mtl typeclasses
class Monad m => MonadIntervalRunner (m :: * -> *) where
  start :: forall o. TimerConf -> Output () -> m o -> o -> m (Input o)

class Monad m => MonadShell m where
  execSh :: Text -> m Text
  default execSh :: (MonadTrans t, MonadShell m1, m ~ t m1) => Text -> m Text
  execSh = lift . execSh

-- Tasks query the system for information
data Task a =
    NopTask a
  | ShellTask Text (Text -> a)
  deriving Functor

-- Providers

-- Providers control how frequently actions are run
-- for now, just via polling on an interval
data ProviderAtom a =
  ProviderAtom
  { _providerConf :: TimerConf
  , _providerTask :: Task a
  , _providerDefault :: a
  }
  deriving Functor

makeLenses ''ProviderAtom

newtype Provider a = Provider (A.Ap ProviderAtom a)
  deriving (Functor, Applicative)

