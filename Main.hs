{-# LANGUAGE LambdaCase, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, NoImplicitPrelude, OverloadedStrings, DefaultSignatures, GADTs, KindSignatures, FlexibleInstances, TemplateHaskell, TypeFamilies, FlexibleContexts, ScopedTypeVariables, DerivingStrategies, DerivingVia, InstanceSigs, StandaloneDeriving, UndecidableInstances, RankNTypes #-}

module Main where

import Relude
import System.Clock
import Data.Semigroup ((<>))
import Control.Lens
import Control.Concurrent (threadDelay)
import UnliftIO.Concurrent (forkIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Concurrent.Async (async)
import Control.Concurrent.Async.Timer (withAsyncTimer, TimerConf, defaultConf, setInitDelay, setInterval, Timer, wait)
import Brick.BChan
import Control.Monad.IO.Unlift
import Control.Applicative.Free as A
import Pipes (Consumer, Producer, runEffect, (>->), yield, await)
import Pipes.Concurrent (spawn, latest, toOutput, fromInput, unbounded, newest, Input, Output)
import qualified Pipes.Prelude as P

newtype Environment = Environment Int

newtype M a = M (ReaderT Environment IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadMask, MonadCatch, MonadThrow)

--class Monad m => MonadChan chan m where
  --newChan :: Int -> m (chan a)
  ---- these block
  --writeChan :: chan a -> a -> m ()
  --readChan :: chan a -> m a

class Monad m => MonadIntervalRunner (m :: * -> *) where
  start :: forall o. TimerConf -> Output () -> m o -> o -> m (Input o)

instance MonadIntervalRunner M where
  start :: forall o. TimerConf -> Output () -> M o -> o -> M (Input o)
  start conf triggerOutput task z = do
    (output, input) <- liftIO $ spawn (latest z)
    _tid <- forkIO $
      withAsyncTimer conf $ \timer ->
        runEffect $ runTask timer >-> toOutput (output <> contramap (const ()) triggerOutput)
    return input
      where
        runTask :: Timer -> Producer o M ()
        runTask timer = do
          lift $ liftIO $ wait timer
          o <- lift task
          yield o

class Monad m => MonadShell m where
  execSh :: Text -> m Text
  default execSh :: (MonadTrans t, MonadShell m1, m ~ t m1) => Text -> m Text
  execSh = lift . execSh

instance MonadShell M where
  execSh s =
    liftIO $ return $ s <> ":test"

-- Tasks query the system for information
data Task a =
    NopTask a
  | ShellTask Text (Text -> a)
  deriving Functor

shell :: Text -> Task Text
shell s = ShellTask s id

runTask :: (MonadIO m, MonadShell m) => Task o -> m o
runTask (ShellTask s f) = do
  liftIO $ threadDelay 100000
  f <$> execSh s

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

newtype ProviderRuntime m a = ProviderRuntime (m (Input a))
  deriving Functor

instance Applicative (ProviderRuntime M) where
  pure a = ProviderRuntime $ do
    (output, input) <- liftIO $ spawn (latest a)
    return input

  liftA2 f (ProviderRuntime ma) (ProviderRuntime mb) = ProviderRuntime $ do
    ia <- ma
    ib <- mb
    return (f <$> ia <*> ib)

after :: Int -> TimerConf
after i = defaultConf & setInitDelay i

everyi :: Int -> TimerConf -> TimerConf
everyi = setInterval

provide :: TimerConf -> Task o -> o -> Provider o
provide conf task z = Provider $ liftAp (ProviderAtom conf task z)

runProvider :: (MonadIntervalRunner m, MonadUnliftIO m, MonadShell m, Applicative (ProviderRuntime m)) => forall o. Output () -> Provider o -> m (Input o)
runProvider triggerOutput (Provider freeAp) = minput
  where
    (ProviderRuntime minput) = runAp (\atom -> ProviderRuntime $ do
      let task' = runTask $ task atom
      start (conf atom) triggerOutput task' (z atom)) freeAp

    conf :: ProviderAtom a -> TimerConf
    conf = view providerConf

    z :: ProviderAtom a -> a
    z = view providerDefault

    task :: ProviderAtom a -> Task a
    task = view providerTask

-- TODO
-- newtype Component a view = Component { consume :: a -> view }

app :: M ()
app = do
  -- we send unit to unblock so we can use latest
  (outputU, inputU) <- liftIO $ spawn (newest 1)

  let tupled = (,) <$> provide (after 0 & everyi 2000) (shell "kwm...") "?" <*> provide (after 200 & everyi 1500) (shell "foo..") "?"

  inputG <- runProvider outputU tupled

  liftIO $ runEffect $ fromInput inputG >-> handler inputU
  --traverse_ wait [a1, a2]
  where
    handler :: (Show i, Show s) => Input () -> Consumer (i, s) IO r
    handler unitInput = forever $ do
      lift $ runEffect $ fromInput unitInput >-> handleUnit
      (i, s) <- await
      print (i, s)
      where
        handleUnit :: Consumer () IO ()
        handleUnit = await

runApp :: M () -> IO ()
runApp (M m) = runReaderT m (Environment 0)

main :: IO ()
main = runApp app
  --c1 <- runProvider $ provide (after 500 & everyi 1000) (shell "echo hello") "?"
  --c2 <- runProvider $ provide (after 250 & everyi 2200) (shell "ls goodbye") "?"
  --forM_ [1..20] $ \_ -> do
    --res <- readChan c1
    --putStrLn (toString res)
    --res <- readChan c2
    --putStrLn (toString res)

