{-# LANGUAGE FlexibleContexts, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, DeriveFunctor, DerivingStrategies, GeneralizedNewtypeDeriving, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- Test CI

module Main where

import System.Clock

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
main = print 4

