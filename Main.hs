{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, ScopedTypeVariables, DataKinds, PolyKinds, DeriveFunctor #-}

module Main where

import System.Clock
import Polysemy
import Polysemy.Input
import Polysemy.Output

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
newtype Provider m a = Provider { providerPoll :: () -> Action m a }
  deriving Functor

data Timeout = Immediate | In TimeSpec

now :: [Timeout]
now = [Immediate]

every :: TimeSpec -> [Timeout]
every s = In s : every s

-- provide :: forall (m :: * -> *) a. [Timeout] -> Action m a -> Provider m a

newtype Component a view = Component { consume :: a -> view }

-- provide (now <> every (5 seconds)) (run "kwm...")
--  <*> 

main :: IO ()
main = do
   out <- runM . shellToIO $ echo
   putStrLn out

