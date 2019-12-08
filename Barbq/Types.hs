{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Barbq.Types
  ( Environment (..),
    M (..),
    DebugState (..),
    Task (..),
    ProviderAtom (..),
    providerConf,
    providerTask,
    Provider (..),
    PointedFinSet,
    point,
    maxSet,
    mkPointedFinSet,
    ScrollyInput
    )
where

import Control.Applicative.Free as A
import Control.Concurrent.Async.Timer (TimerConf)
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Data.Text.Lazy
import Relude hiding (Text)

data DebugState
  = Prod
  | Debug

-- Base Monad
newtype Environment = Environment DebugState

newtype M a = M (ReaderT Environment IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadMask, MonadCatch, MonadThrow, MonadReader Environment)

-- Tasks query the system for information
data Task a
  = NopTask a
  | ShellTask Text (Text -> a)
  | IOTask (IO a)
  deriving (Functor)

-- Providers

-- Providers control how frequently actions are run
-- for now, just via polling on an interval
data ProviderAtom a
  = ProviderAtom
      { _providerConf :: TimerConf,
        _providerTask :: Task a
        }
  deriving (Functor)

makeLenses ''ProviderAtom

newtype Provider a = Provider (A.Ap ProviderAtom (Maybe a))
  deriving (Functor)

instance Applicative Provider where

  pure = Provider . A.Pure . Just

  liftA2 op (Provider fa) (Provider fb) =
    Provider $ (\a b -> op <$> a <*> b) <$> fa <*> fb

-- Models
newtype PointedFinSet = PointedFinSet (Int, Int)
  deriving (Show)

mkPointedFinSet :: Int -> Int -> PointedFinSet
mkPointedFinSet point max = PointedFinSet (point, max)

point :: PointedFinSet -> Int
point (PointedFinSet tuple) = fst tuple

maxSet :: PointedFinSet -> Int
maxSet (PointedFinSet tuple) = snd tuple

type ScrollyInput = (Text, Sum Int, Int, Int)
