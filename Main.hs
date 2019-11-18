{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Barbq.Types
import Barbq.UI
import Control.Applicative.Free as A
import Control.Comonad.Store
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.Async.Timer (Timer, TimerConf, defaultConf, setInitDelay, setInterval, wait, withAsyncTimer)
import Control.Lens
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Co
import Control.Monad.IO.Unlift
import Data.Semigroup ((<>))
import Data.Text.Lazy
import Data.Text.Lazy.Read
import Graphics.Vty ((<|>), blue, defAttr, green, mkVty, nextEvent, picForImage, shutdown, standardIOConfig, string, text, update, withBackColor, withForeColor)
import Pipes ((>->), Consumer, Pipe, Producer, await, runEffect, yield)
import Pipes.Concurrent (Input, Output, fromInput, latest, newest, spawn, toOutput, unbounded)
import qualified Pipes.Prelude as P
import Relude hiding ((<|>), Text)
import System.Clock
import System.Process (readProcess)
import Text.RawString.QQ
import UnliftIO.Concurrent (forkIO)

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
  execSh s = do
    output <- liftIO $ readProcess "/bin/bash" ["-c", unpack s] []
    return $ pack output

-- use Turtle to make shells?
shell :: Text -> Task Text
shell = flip ShellTask id

-- scripts taken from ubersicht status widget via chunkwm sample ubersicht
volumeShell :: Text
volumeShell =
  [r|
  isMute=$( /usr/bin/osascript -e 'output muted of (get volume settings)' )

  if [ $isMute == "true" ]; then
    echo "0"
  else
    curVolume=$(osascript -e 'output volume of (get volume settings)')
    echo $curVolume
  fi
|]

wifiShell :: Text
wifiShell =
  [r|
  services=$(networksetup -listnetworkserviceorder | grep 'Hardware Port')

while read line; do
    sname=$(echo $line | awk -F  "(, )|(: )|[)]" '{print $2}')
    sdev=$(echo $line | awk -F  "(, )|(: )|[)]" '{print $4}')
    # echo "Current service: $sname, $sdev, $currentservice"
    if [ -n "$sdev" ]; then
        ifconfig $sdev 2>/dev/null | grep 'status: active' > /dev/null 2>&1
        rc="$?"
        if [ "$rc" -eq 0 ]; then
            currentservice="$sname"
	    currentsdev="$sdev"
            break
        fi
    fi
done <<< "$(echo "$services")"

if [ -n "$currentservice" ] ; then
    echo "$currentservice@$(networksetup -getairportnetwork $currentsdev | cut -c 24-)@$(networksetup -getinfo "Apple USB Ethernet Adapter" | grep "IP address" | grep "\." | cut -c 13-)"
else
    >&1 echo "none@none@"
    exit 1
fi
|]

shellInt :: Text -> Task (Maybe Int)
shellInt s =
  let taskEither = decimal <$> shell s
      taskMaybe = rightToMaybe <$> taskEither
   in fmap fst <$> taskMaybe

runTask :: (MonadIO m, MonadShell m) => Task o -> m o
runTask (ShellTask s f) = do
  liftIO $ threadDelay 100000
  f <$> execSh s

newtype ProviderRuntime m a = ProviderRuntime (m (Input a))
  deriving (Functor)

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
    (ProviderRuntime minput) =
      runAp
        ( \atom -> ProviderRuntime $ do
            let task' = runTask $ task atom
            start (conf atom) triggerOutput task' (z atom)
          )
        freeAp
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
  cfg <- liftIO standardIOConfig
  vty <- liftIO $ mkVty cfg
  -- we send unit to unblock so we can use latest
  (outputU, inputU) <- liftIO $ spawn (newest 1)
  -- (purely) describe the providers
  --let tupled = (\a b c d -> (a, b, c, d)) <$> provide (after 1000 & everyi 2000) (shell "kwm...") "?" <*> provide (after 300 & everyi 1500) (shell "foo..") "?" <*> provide (after 200 & everyi 4000) (shell "bar..") "?" <*> provide (after 20000 & everyi 40000) (shell "bar..") "?"
  let left :: Provider Int = fromMaybe 0 <$> provide (after 0 & everyi 500) (shellInt volumeShell) Nothing
  let right :: Provider (Maybe Text) = provide (after 1000 & everyi 2000) (Just <$> shell wifiShell) Nothing
  let tupled = (,) <$> left <*> right
  -- run the provider
  inputG <- runProvider outputU tupled
  input <- liftIO $ normalize inputG inputU
  liftIO $ runRenderM (exploreCo realComponent) vty input
  -- dump provided data to stdout
  liftIO $ shutdown vty
  where
    -- Given a "latest" input and a unit trigger, yield a triggered latest
    normalize :: Input a -> Input () -> IO (Input (Maybe a))
    normalize latestInput triggerInput = do
      (output, input) <- spawn (newest 1)
      _tid <- forkIO $ runEffect $ fromInput latestInput >-> handler triggerInput >-> toOutput output
      return input
    handler :: Input () -> Pipe a (Maybe a) IO ()
    handler triggerInput = loop 0
      where
        loop i = do
          lift $ runEffect $ fromInput triggerInput >-> await
          a <- await
          yield (Just a)
          if i > 10
            then yield Nothing
            else loop (i + 1)

runApp :: M () -> IO ()
runApp (M m) = runReaderT m (Environment 0)

main :: IO ()
main = runApp app
