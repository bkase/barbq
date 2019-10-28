{-# LANGUAGE LambdaCase, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, NoImplicitPrelude, OverloadedStrings, DefaultSignatures, GADTs, KindSignatures, FlexibleInstances, TemplateHaskell, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Relude
import System.Clock
import Data.Semigroup ((<>))
import Control.Monad.Conc.Class
import Control.Lens
import Control.Concurrent.Async.Timer
import Brick.BChan
import Control.Monad.IO.Unlift

class Monad m => MonadChan chan m where
  newChan :: Int -> m (chan a)
  -- these block
  writeChan :: chan a -> a -> m ()
  readChan :: chan a -> m a

instance (Monad m, MonadIO m) => MonadChan BChan m where
  newChan = liftIO . newBChan
  writeChan chan = liftIO . writeBChan chan
  readChan = liftIO . readBChan

class Monad m => MonadIntervalRunner chan o (m :: * -> *) where
  start :: chan o -> TimerConf -> m o -> m (ThreadId m)

instance (Monad m, MonadConc m, MonadChan BChan m, MonadUnliftIO m) => MonadIntervalRunner BChan o m where
  start chan conf task =
    fork $
    withAsyncTimer conf $ \timer ->
      forever $ do
        wait timer
        o <- task
        writeChan chan o

class Monad m => MonadShell m where
  execSh :: Text -> m Text
  default execSh :: (MonadTrans t, MonadShell m1, m ~ t m1) => Text -> m Text
  execSh = lift . execSh

instance MonadShell IO where
  execSh s =
    return $ s <> ":test"

-- Tasks query the system for information
data Task o = ShellTask Text (Text -> o)
  deriving Functor

shell :: Text -> Task Text
shell s = ShellTask s id

runTask :: MonadShell m => Task o -> m o
runTask (ShellTask s f) = f <$> execSh s

-- Providers control how frequently actions are run
-- for now, just via polling on an interval
data Provider o =
  Provider
  { _providerConf :: TimerConf
  , _providerTask :: Task o
  , _providerDefault :: o
  }

makeLenses ''Provider

after :: Int -> TimerConf
after i = defaultConf & setInitDelay i

every :: Int -> TimerConf -> TimerConf
every = setInterval

provide :: TimerConf -> Task o -> o -> Provider o
provide conf task z = Provider conf task z

runProvider :: (MonadIntervalRunner BChan o m, MonadChan BChan m, MonadConc m, MonadUnliftIO m, MonadShell m) => Provider o -> m (BChan o)
runProvider provider = do
  chan <- newChan 30
  _tid <- start chan (view providerConf provider) (runTask $ (_providerTask provider))
  return chan

newtype Component a view = Component { consume :: a -> view }

-- provide (now <> every (5 seconds)) (run "kwm...")
--  <*> 

main :: IO ()
main = do
  c1 <- runProvider $ provide (after 500 & every 1000) (shell "echo hello") "?"
  c2 <- runProvider $ provide (after 250 & every 2200) (shell "ls goodbye") "?"
  forM_ [1..20] $ \_ -> do
    res <- readChan c1
    putStrLn (toString res)
    res <- readChan c2
    putStrLn (toString res)

