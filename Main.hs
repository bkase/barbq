{-# LANGUAGE LambdaCase, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveFunctor, NoImplicitPrelude, OverloadedStrings, DefaultSignatures, GADTs, KindSignatures, FlexibleInstances, TemplateHaskell #-}

module Main where

import Relude
import System.Clock
import Data.Semigroup ((<>))
import Control.Concurrent.Classy.Async
import Control.Monad.Conc.Class
import Control.Lens

data Timeout = Immediate | In Int -- micors

now :: [Timeout]
now = [Immediate]

every :: Int -> [Timeout]
every s = In s : every s

toMicros :: Timeout -> Int
toMicros Immediate = 0
toMicros (In amt) = amt

class Monad m => MonadIntervalRunner o (m :: * -> *) where
  latest :: m (Maybe o)
  start  :: m o -> m (Async m o)

data RunnerState o = RunnerState { _rsTimeouts :: [Timeout], _rsLatest :: Maybe o }
  deriving Functor

makeLenses ''RunnerState

instance (Monad m, MonadConc m) => MonadIntervalRunner o (StateT (RunnerState o) m) where
  latest = view rsLatest <$> get

  start task = do
    state <- get
    let next:rest = view rsTimeouts state
    put $ set rsTimeouts rest state

    async $ do
      threadDelay $ toMicros next
      task

class Monad m => MonadShell m where
  execSh :: Text -> m Text
  default execSh :: (MonadTrans t, MonadShell m1, m ~ t m1) => Text -> m Text
  execSh = lift . execSh

echo :: MonadShell m => Text -> m Text
echo s = execSh $ "echo " <> s

instance MonadShell IO where
  execSh s = do
    line <- getLine
    return $ s <> ":" <> line

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
  res <- echo "helloWorld"
  putStrLn (toString res)

