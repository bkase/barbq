{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Derived from https://github.com/paf31/purescript-react-explore
module Barbq.UI.Framework
  ( pairStateStore,
    pairWriterTraced,
    pairIdentity,
    pairSym,
    pairCo,
    pairDay,
    componentMapAction,
    Pairing,
    Component,
    Component' (..),
    Handler,
    UI (..),
    combine,
    stateToCoStore,
    writerToCoTraced
    )
where

import Control.Comonad.Store
import Control.Comonad.Traced
import Control.Monad.Co
import Control.Monad.Writer (Writer, WriterT (..), mapWriter, runWriter)
import Control.Newtype
import Data.Functor.Day
import Data.Profunctor
import Relude hiding ((<|>), Text, filter)

-- Pairings
type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

pairCo :: Functor f => Pairing f (Co f)
pairCo f fa cofb = runCo cofb (f <$> fa)

pairSym :: forall f g. Pairing f g -> Pairing g f
pairSym pairing f ga fb = pairing (flip f) fb ga

pairDay :: forall f1 f2 g1 g2. Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Day f1 g1) (Day f2 g2)
pairDay p1 p2 f (Day f1 g1 g) (Day f2 g2 h) =
  let (x1, x2) = p1 (,) f1 f2
      (y1, y2) = p2 (,) g1 g2
   in f (g x1 y1) (h x2 y2)

pairIdentity :: Pairing Identity Identity
pairIdentity f fa gb = runIdentity (liftA2 f fa gb)

pairStateStore :: forall f g s. Pairing f g -> Pairing (StateT s f) (StoreT s g)
pairStateStore pairing f (StateT state) (StoreT gf s) =
  pairing (\(a, s1) f1 -> f a (f1 s1)) (state s) gf

pairWriterTraced :: forall f g s. Pairing f g -> Pairing (WriterT s f) (TracedT s g)
pairWriterTraced pairing f (WriterT writer) (TracedT gf) =
  pairing (\(a, w) f1 -> f a (f1 w)) writer gf

-- Components

-- Supply changes to `s` in response to an action `a`
type Handler a s = a -> Endo s

-- Describe how to render a view, `v`, and optionally tell the writer how certain
-- events, `e`, trigger specific actions, `a`, by calling the `Handler a s`.
-- This is a component frozen at some particular moment
newtype UI a e v = UI (forall s. (Handler a s -> Writer (Handler e s) v))

-- Contravariant over events, covariant over views
-- Note: UIs are also covariant over the actions, but I haven't needed to use that yet and am unsure the right way to expose that
instance Profunctor (UI a) where

  lmap :: forall a v e1 e2. (e2 -> e1) -> UI a e1 v -> UI a e2 v
  lmap f (UI ui) = UI $ \send -> mapWriter writerMap (ui send)
    where
      writerMap :: (v, Handler e1 s) -> (v, Handler e2 s)
      writerMap (img, r) = (img, r <<< f)

  rmap :: forall a e v1 v2. (v1 -> v2) -> UI a e v1 -> UI a e v2
  rmap f (UI ui) = UI $ fmap f . ui

-- A component is a pointer to a current UI (at this moment) and a space of all
-- possible other UIs (given to us by `w`). Actions under some monad `m` must
-- pair with `w` in order to explore the space.
newtype Component' w m e v = Component' (w (UI (m ()) e v))

instance Newtype (Component' w m e v) (w (UI (m ()) e v))

instance Functor w => Profunctor (Component' w m) where
  dimap f g (Component' c1) = Component' $ dimap f g <$> c1

-- We can use `Control.Monad.Co` to find the pairing monad for us
type Component w e v = Component' w (Co w) e v

componentMapAction
  :: forall m1 m2 w e v. Functor w
  => (m1 () -> m2 ())
  -> Component' w m1 e v
  -> Component' w m2 e v
componentMapAction f = over Component' $ fmap transformUi
  where
    transformUi :: UI (m1 ()) e v -> UI (m2 ()) e v
    transformUi (UI ui) = UI $ \send -> ui (send <<< f)

stateToCoStore :: forall a s. State s a -> Co (Store s) a
stateToCoStore state = co (story state)
  where
    story :: forall r. State s a -> Store s (a -> r) -> r
    story state store =
      let (render, s) = runStore store
          (a, s') = runState state s
       in render s' a

writerToCoTraced :: forall a s. Writer s a -> Co (Traced s) a
writerToCoTraced writer = co (tracer writer)
  where
    tracer :: forall r. Writer s a -> Traced s (a -> r) -> r
    tracer writer traced =
      let (a, s) = runWriter writer
          f = runTraced traced s
       in f a

-- Place two components next to one another
combine
  :: forall w1 w2 e v. Comonad w1
  => Comonad w2
  => (forall a. UI a e v -> UI a e v -> UI a e v)
  -> Component w1 e v
  -> Component w2 e v
  -> Component (Day w1 w2) e v
combine with (Component' w1) = over Component' $ day (build <$> w1)
  where
    build :: UI (Co w1 ()) e v -> UI (Co w2 ()) e v -> UI (Co (Day w1 w2) ()) e v
    build (UI render1) (UI render2) =
      with (UI (\send -> render1 $ \co -> send (liftLeft co)))
        (UI (\send -> render2 $ \co -> send (liftRight co)))

liftLeft :: forall w w' a. Functor w => Comonad  w' => Co w a -> Co (Day w  w') a
liftLeft a = co (\(Day w w' f) -> runCo a (fmap (`f` extract w') w))

liftRight :: forall w w' a. Functor w => Comonad  w' => Co w a -> Co (Day  w' w) a
liftRight a = co (\(Day w' w f) -> runCo a (fmap (f (extract w')) w))
