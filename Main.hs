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

data Timeout = Immediate | In Int -- micors

now :: [Timeout]
now = [Immediate]

every :: Int -> [Timeout]
every s = In s : every s

toMicros :: Timeout -> Int
toMicros Immediate = 0
toMicros (In amt) = amt

class Monad m => MonadChan chan m where
  newChan :: Int -> m (chan a)
  -- these block
  writeChan :: chan a -> a -> m ()
  readChan :: chan a -> m a

instance (Monad m, MonadIO m) => MonadChan BChan m where
  newChan capacity = liftIO (newBChan capacity)
  writeChan chan a = liftIO (writeBChan chan a)
  readChan chan = liftIO (readBChan chan)

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

echo :: MonadShell m => Text -> m Text
echo s = execSh $ "echo " <> s

instance MonadShell IO where
  execSh s =
    return $ s <> ":test"

-- Actions get an `a` from the universe
newtype Action m a = Action (m a)
  deriving Functor

-- Providers control how frequently actions are run
-- for now, just via polling

-- provide :: forall (m :: * -> *) a. [Timeout] -> Action m a -> Provider m a

newtype Component a view = Component { consume :: a -> view }

-- provide (now <> every (5 seconds)) (run "kwm...")
--  <*> 

main :: IO ()
main = do
  (c :: BChan Text) <- newChan 10
  let conf1 = defaultConf & setInitDelay  500 -- 500 ms
                          & setInterval  1000 -- 1 s
  let conf2 = defaultConf & setInitDelay  250 -- 250 ms
                          & setInterval  2200 -- 2 s
  id1 <- start c conf1 (echo "echo1")
  id2 <- start c conf2 (echo "echo2")
  forM_ [1..20] $ \_ -> do
    res <- readChan c
    putStrLn (toString res)

