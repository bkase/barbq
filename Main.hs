{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Barbq.Types
import Barbq.UI.Components
import Barbq.UI.Runtime (exploreCo, runRenderM)
import Control.Applicative.Free as A
import Control.Comonad.Store
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Timer (Timer, TimerConf, defaultConf, setInitDelay, setInterval, wait, withAsyncTimer)
import Control.Lens
import Control.Monad.IO.Unlift
import Data.Semigroup ((<>))
import Data.Text.Lazy
import Graphics.Vty (mkVty, shutdown, standardIOConfig)
import Pipes ((>->), Pipe, Producer, await, runEffect, yield)
import Pipes.Concurrent (Input, Output, fromInput, latest, newest, spawn, toOutput)
import Relude hiding ((<|>), Text)
import System.Process (readProcess)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.RawString.QQ
import UnliftIO.Concurrent (forkIO)

-- Mtl typeclasses
class Monad m => MonadIntervalRunner (m :: * -> *) where

  start :: forall o. TimerConf -> Output () -> m o -> m (Input (Maybe o))

class Monad m => MonadShell m where

  execSh :: Text -> m Text

  default execSh :: (MonadTrans t, MonadShell m1, m ~ t m1) => Text -> m Text
  execSh = lift . execSh

instance MonadIntervalRunner M where
  start :: forall o. TimerConf -> Output () -> M o -> M (Input (Maybe o))
  start conf triggerOutput task = do
    (output, input) <- liftIO $ spawn (latest Nothing)
    _tid <- forkIO $ withAsyncTimer conf (loop output)
    return input
    where
      loop output timer = do
        runEffect $ runTask timer >-> toOutput (output <> contramap (const ()) triggerOutput)
        loop output timer
      runTask :: Timer -> Producer (Maybe o) M ()
      runTask timer = do
        lift $ liftIO $ wait timer
        o <- lift task
        yield $ Just o

instance MonadShell M where
  execSh s = do
    output <- liftIO $ readProcess "/bin/bash" ["-c", unpack s] []
    -- trace output $
    return $ pack output

shell :: Text -> Task Text
shell = flip ShellTask id

tilingShell :: Text
tilingShell =
  [r|
  desktops=$(chunkc tiling::query -D $(chunkc tiling::query -m id))
  current=$(/usr/local/bin/chunkc tiling::query -d id)

  echo "$current,${desktops: -1}"
|]

-- scripts taken from ubersicht status widget via chunkwm sample ubersicht
volumeShell :: Text
volumeShell =
  [r| /Users/bkase/barbq2/getvolume/.build/debug/getvolume |]

externalIpShell :: Text
externalIpShell = "curl -s icanhazip.com"

internalIpShell :: Text
internalIpShell = "ipconfig getifaddr en0"

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
    echo -n "$currentservice: $(networksetup -getairportnetwork $currentsdev | cut -c 24-)"
else
    >&1 echo -n "Unknown"
    exit 1
fi
|]

shellInt :: Text -> Task (Maybe Int)
shellInt s =
  runParser <$> shell s
  where
    runParser :: Text -> Maybe Int
    runParser s = rightToMaybe $ parse parser "" s
      where
        parser :: Parser Int
        parser = decimal

-- TODO: Make the int more composable or something
runTask :: (MonadIO m, MonadShell m) => Task o -> m o
runTask (ShellTask s f) = do
  liftIO $ threadDelay 100000
  f <$> execSh s
runTask (NopTask a) = return a
runTask (IOTask ma) = liftIO ma

newtype ProviderRuntime m a = ProviderRuntime (m (Input (Maybe a)))
  deriving (Functor)

instance Applicative (ProviderRuntime M) where

  pure a = ProviderRuntime $ do
    (_output, input) <- liftIO $ spawn (latest $ Just a)
    return input

  liftA2 f (ProviderRuntime ma) (ProviderRuntime mb) = ProviderRuntime $ do
    ia <- ma
    ib <- mb
    return $ (\a b -> f <$> a <*> b) <$> ia <*> ib

after :: Int -> TimerConf
after i = defaultConf & setInitDelay i

everyi :: Int -> TimerConf -> TimerConf
everyi = setInterval

provide :: TimerConf -> Task o -> Provider o
provide conf task = Provider $ Just <$> liftAp (ProviderAtom conf task)

runProvider :: (MonadIntervalRunner m, MonadUnliftIO m, MonadShell m, Applicative (ProviderRuntime m)) => forall o. Output () -> Provider o -> m (Input (Maybe o))
runProvider triggerOutput (Provider freeAp) = fmap join <$> minput
  where
    (ProviderRuntime minput) =
      runAp
        ( \atom -> ProviderRuntime $ do
            let task' = runTask $ task atom
            start (conf atom) triggerOutput task'
          )
        freeAp
    conf :: ProviderAtom a -> TimerConf
    conf = view providerConf
    task :: ProviderAtom a -> Task a
    task = view providerTask

type Parser = Parsec Void Text

tabsTask :: Task (Maybe PointedFinSet)
tabsTask = fmap (uncurry mkPointedFinSet) <$> fixed
  where
    fixed :: Task (Maybe (Int, Int))
    fixed = untext <$> shell tilingShell
    untext :: Text -> Maybe (Int, Int)
    untext s = rightToMaybe $ parse numbers "" s
      where
        numbers :: Parser (Int, Int)
        numbers = do
          point <- decimal
          _ <- char ','
          max <- decimal
          pure (point, max)

scrollTask :: IORef (Sum Int) -> Task (Sum Int)
scrollTask ref = IOTask $ do
  x <- readIORef ref
  let next = x <> Sum 1
  writeIORef ref next
  return next

app :: M ()
app = do
  cfg <- liftIO standardIOConfig
  vty <- liftIO $ mkVty cfg
  -- we send unit to unblock so we can use latest
  (outputU, inputU) <- liftIO $ spawn (newest 1)
  -- (purely) describe the providers
  let volumeData :: Provider Int = provide (after 0 & everyi 500) (fromMaybe 0 <$> shellInt volumeShell)
  let wifiData :: Provider Text = provide (after 0 & everyi 2000) (shell wifiShell)
  let tabsData :: Provider (Maybe PointedFinSet) = provide (after 0 & everyi 100) tabsTask
  ref <- liftIO $ newIORef mempty
  let scrollData :: Provider ScrollyInput = ("Billie Eilish - Ocean Eyes",,20,5) <$> provide (after 0 & everyi 500) (scrollTask ref)
  let tupled = (,,,) <$> tabsData <*> volumeData <*> wifiData <*> scrollData
  -- run the provider
  inputG <- runProvider outputU tupled
  input <- liftIO $ normalize inputG inputU
  liftIO $ runRenderM (exploreCo realComponent) vty input
  liftIO $ shutdown vty
  where
    -- Given a "latest" input and a unit trigger, yield a triggered latest
    normalize :: Show a => Input a -> Input () -> IO (Input (Maybe a))
    normalize latestInput triggerInput = do
      (output, input) <- spawn (newest 1)
      _tid <- forkIO $ runEffect $ fromInput latestInput >-> handler triggerInput >-> toOutput output
      return input
    -- wait for the trigger and yield Just until a count threshold
    handler :: Show a => Input () -> Pipe a (Maybe a) IO ()
    handler triggerInput = loop 0
      where
        loop :: Show a => Int -> Pipe a (Maybe a) IO ()
        loop i = do
          lift $ runEffect $ fromInput triggerInput >-> await
          a <- await
          yield (Just a)
          loop (i + 1)

runApp :: M () -> IO ()
runApp (M m) = runReaderT m (Environment 0)

main :: IO ()
main = runApp app
