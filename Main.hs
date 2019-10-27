{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, ScopedTypeVariables, DataKinds, PolyKinds, DeriveFunctor #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import System.Clock
import Polysemy
import Polysemy.Internal (send)
import Polysemy.Input
import Polysemy.Output

data Timeout = Immediate | In TimeSpec

now :: [Timeout]
now = [Immediate]

every :: TimeSpec -> [Timeout]
every s = In s : every s

data Scheduler m a where
  Schedule :: Timeout -> (a -> m ()) -> Scheduler m a

makeSem ''Scheduler

data Provider o m a where
  Latest :: Provider o m (Maybe o)
  Start :: [Timeout] -> m a -> Provider o m ()

makeSem ''Provider

doSomething :: Member (Provider o) r => Sem r a -> Sem r ()
doSomething ma = start [] ma

--latest :: forall o r. Member (Provider o) r => Sem r (Maybe o)
--latest = send (Latest :: Provider o (Sem r) (Maybe o))

--start :: forall o r a. Member (Provider o) r => [Timeout] -> Sem r a -> Provider o (Sem r) a
--start ts ma = send (Start ts ma :: Provider o (Sem r) a)

-- providerToScheduler :: Members '[Scheduler, 

data Shell m a where
  ExecSh :: String -> Shell m String

makeSem ''Shell

echo :: Member Shell r => Sem r String
echo = execSh "hello world"

shellToIO :: Member (Embed IO) r => Sem (Shell ': r) a -> Sem r a
shellToIO = interpret $ \case
  ExecSh s -> embed $ do
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
   out <- runM . shellToIO $ echo
   putStrLn out

