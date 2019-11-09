{-# LANGUAGE LambdaCase, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, NoImplicitPrelude, OverloadedStrings, DefaultSignatures, GADTs, KindSignatures, FlexibleInstances, TemplateHaskell, TypeFamilies, FlexibleContexts, ScopedTypeVariables, DerivingStrategies, DerivingVia, InstanceSigs, StandaloneDeriving, UndecidableInstances, RankNTypes #-}

module Main where

import Relude hiding ((<|>))
import Barbq.Types
import Barbq.UI
import System.Clock
import Data.Semigroup ((<>))
import Control.Monad.Co
import Control.Comonad.Store
import Control.Lens
import Control.Concurrent (threadDelay)
import UnliftIO.Concurrent (forkIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Concurrent.Async (async)
import Control.Concurrent.Async.Timer (withAsyncTimer, TimerConf, defaultConf, setInitDelay, setInterval, Timer, wait)
import Graphics.Vty (standardIOConfig, mkVty, string, text, defAttr, withForeColor, green, picForImage, update, nextEvent, shutdown, blue, withBackColor, (<|>))
import Control.Monad.IO.Unlift
import Control.Applicative.Free as A
import Pipes (Consumer, Producer, runEffect, (>->), yield, await)
import Pipes.Concurrent (spawn, latest, toOutput, fromInput, unbounded, newest, Input, Output)
import qualified Pipes.Prelude as P

--class Monad m => MonadChan chan m where
  --newChan :: Int -> m (chan a)
  ---- these block
  --writeChan :: chan a -> a -> m ()
  --readChan :: chan a -> m a

instance MonadIntervalRunner M where
  start :: forall o. TimerConf -> Output () -> M o -> o -> M (Input o)
  start conf triggerOutput task z = do
    (output, input) <- liftIO $ spawn (latest z)
    _tid <- forkIO $ withAsyncTimer conf (loop output)
    return input
      where
        loop output timer = do
          runEffect $ runTask timer >-> toOutput (output <> contramap (const ()) triggerOutput)
          loop output timer

        runTask :: Timer -> Producer o M ()
        runTask timer = do
          lift $ liftIO $ wait timer
          o <- lift task
          yield o

instance MonadShell M where
  execSh s =
    liftIO $ return $ s <> ":test"

shell :: Text -> Task Text
shell = flip ShellTask id

runTask :: (MonadIO m, MonadShell m) => Task o -> m o
runTask (ShellTask s f) = do
  liftIO $ threadDelay 100000
  f <$> execSh s

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
-- type UI a = (a -> Widget ()

--type Component w = w (UI (Co w ()))

--counter :: Component (Store Int)
--counter = store render 0
  --where
    --render :: Int -> UI (Co (Store Int) ())
    --render count send =
      --send "hello"

app :: M ()
app = do
  -- we send unit to unblock so we can use latest
  (outputU, inputU) <- liftIO $ spawn (newest 1)

  -- (purely) describe the providers
  let tupled = (\a b c d -> (a,b,c,d)) <$> provide (after 1000 & everyi 2000) (shell "kwm...") "?" <*> provide (after 300 & everyi 1500) (shell "foo..") "?" <*> provide (after 200 & everyi 4000) (shell "bar..") "?" <*> provide (after 20000 & everyi 40000) (shell "bar..") "?"

  -- run the provider
  inputG <- runProvider outputU tupled

  -- dump provided data to stdout
  liftIO $ runEffect $ fromInput inputG >-> handler inputU
  where
    handler :: (Show a) => Input () -> Consumer a IO r
    handler unitInput = forever $ do
      lift $ runEffect $ fromInput unitInput >-> await
      a <- await
      print a

runApp :: M () -> IO ()
runApp (M m) = runReaderT m (Environment 0)

main :: IO ()
main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  flip runRenderM vty $ exploreCo combinedExample
  shutdown vty
  print ("Last event was: " ++ show 3)
  runApp app

