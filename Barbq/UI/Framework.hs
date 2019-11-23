{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barbq.UI.Framework
  ( pairStateStore,
    pairWriterTraced,
    pairIdentity,
    pairSym,
    pairCo,
    pairDay,
    componentMapView,
    componentMapAction,
    componentPullbackEvent,
    Pairing,
    Component,
    Component',
    SendResult,
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
import Control.Monad.Writer (Writer, WriterT(..), mapWriter, runWriter)
import Data.Functor.Day
import Relude hiding ((<|>), Text, filter)

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

type SendResult s = Endo s

type Handler a s = a -> SendResult s

newtype UI e v a = UI (forall s. (Handler a s -> Writer (Handler e s) v))

type Component' e v w m = w (UI e v (m ()))

type Component e v w = Component' e v w (Co w)

componentMapAction
  :: forall m1 m2 w e v. Functor w
  => (m1 () -> m2 ())
  -> Component' e v w m1
  -> Component' e v w m2
componentMapAction f = fmap transformUi
  where
    transformUi :: UI e v (m1 ()) -> UI e v (m2 ())
    transformUi (UI ui) = UI $ \send -> ui (send <<< f)

componentMapView
  :: forall m w e v1 v2. Functor w
  => (v1 -> v2)
  -> Component' e v1 w m
  -> Component' e v2 w m
componentMapView f = fmap transformUi
  where
    transformUi :: UI e v1 (m ()) -> UI e v2 (m ())
    transformUi (UI ui) = UI $ fmap f . ui

componentPullbackEvent
  :: forall m w e1 e2 v. Functor w
  => (e2 -> e1)
  -> Component' e1 v w m
  -> Component' e2 v w m
componentPullbackEvent f = fmap transformUi
  where
    transformUi :: UI e1 v (m ()) -> UI e2 v (m ())
    transformUi (UI ui) = UI $ \send -> mapWriter writerMap (ui send)
    writerMap :: (v, Handler e1 s) -> (v, Handler e2 s)
    writerMap (img, r) = (img, r <<< f)

stateToCoStore :: forall a s. State s a -> Co (Store s) a
stateToCoStore state = co (story state)
  where
    story :: forall r. State s a -> Store s (a -> r) -> r
    story state store =
      let { (render, s) = runStore store }
       in let { (a, s') = runState state s }
           in render s' a

writerToCoTraced :: forall a s. Writer s a -> Co (Traced s) a
writerToCoTraced writer = co (tracer writer)
  where
    tracer :: forall r. Writer s a -> Traced s (a -> r) -> r
    tracer writer traced =
      let { (a, s) = runWriter writer }
       in let { f = runTraced traced s }
           in f a

pairIdentity :: Pairing Identity Identity
pairIdentity f fa gb = runIdentity (liftA2 f fa gb)

pairStateStore :: forall f g s. Pairing f g -> Pairing (StateT s f) (StoreT s g)
pairStateStore pairing f (StateT state) (StoreT gf s) =
  pairing (\(a, s1) f1 -> f a (f1 s1)) (state s) gf

pairWriterTraced :: forall f g s. Pairing f g -> Pairing (WriterT s f) (TracedT s g)
pairWriterTraced pairing f (WriterT writer) (TracedT gf) =
  pairing (\(a, w) f1 -> f a (f1 w)) writer gf

combine
  :: forall w1 w2 e v. Comonad w1
  => Comonad w2
  => (forall a. UI e v a -> UI e v a -> UI e v a)
  -> Component e v w1
  -> Component e v w2
  -> Component e v (Day w1 w2)
combine with w1 = day (build <$> w1)
  where
    build :: UI e v (Co w1 ()) -> UI e v (Co w2 ()) -> UI e v (Co (Day w1 w2) ())
    build (UI render1) (UI render2) =
      with (UI (\send -> render1 $ \co -> send (liftLeft co)))
        (UI (\send -> render2 $ \co -> send (liftRight co)))

liftLeft :: forall w w' a. Functor w => Comonad  w' => Co w a -> Co (Day w  w') a
liftLeft a = co (\(Day w w' f) -> runCo a (fmap (`f` extract w') w))

liftRight :: forall w w' a. Functor w => Comonad  w' => Co w a -> Co (Day  w' w) a
liftRight a = co (\(Day w' w f) -> runCo a (fmap (f (extract w')) w))
