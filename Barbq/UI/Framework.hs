{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

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
    Choice (..),
    Co' (..),
    tell',
    moveTrue,
    moveFalse,
    liftTrue,
    liftFalse,
    combine,
    combineN,
    stack,
    stateToCoStore,
    writerToCoTraced
    )
where

import Control.Comonad.Env as E
import Control.Comonad.Store
import Control.Comonad.Traced as T
import Control.Monad.Co
import Control.Monad.Writer (Writer, WriterT (..), mapWriter, runWriter)
import Control.Newtype
import Data.Functor.Day
import Data.Profunctor hiding (Choice)
import Relude hiding ((<|>), Text, filter)

-- Newtype to get mtl instances off the comonads instead of the CoT transformer
newtype Co' w a = Co' (Co w a)
  deriving (Functor, Applicative, Monad)

instance Newtype (Co' w a) (Co w a)

instance MonadState s (Co' (Store s)) where

  get = Co' $ co (\store -> extract store (pos store))

  put s = Co' $ co (\store -> extract (seek s store) ())

instance MonadReader r (Co' (Env r)) where

  ask = Co' $ co (\env -> extract env (E.ask env))

  local f (Co' (CoT c)) = Co' $ CoT (c <<< E.local f)

-- "MonadTell" instance
tell' :: s -> Co' (Traced s) ()
tell' s = Co' $ co (\t -> runTraced t s ())

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

type family Comonads (ts :: [* -> *]) :: Constraint where
  Comonads '[] = ()
  Comonads (w ': ws) = (Comonad w, Comonads ws)

data Nat' = Zero | Succ Nat'

type family Length xs where
  Length '[] = 'Zero
  Length (_ ': xs) = 'Succ (Length xs)

data DayNel f ds a where
  DayOne :: f a -> DayNel f '[] a
  DayCons :: Day f (DayNel g ds) a -> DayNel f (g ': ds) a

instance Functor f => Functor (DayNel f '[]) where
  fmap f (DayOne fa) = DayOne (f <$> fa)

instance (Functor f, Functor g) => Functor (DayNel f (g ': ds)) where
  fmap f (DayCons day) = DayCons $ fmap f day

instance Comonad w => Comonad (DayNel w '[]) where

  extract (DayOne wa) = extract wa

  duplicate (DayOne wa) = DayOne $ (fmap DayOne $ duplicate wa)

instance (Comonad w1, Functor w2, Comonad (DayNel w2 ds)) => Comonad (DayNel w1 (w2 ': ds)) where

  extract (DayCons wa) = extract wa

  duplicate (DayCons wa) = DayCons $ (fmap DayCons $ duplicate wa)

liftCoDayOne :: forall w a. Comonad w => Co w a -> Co (DayNel w '[]) a
liftCoDayOne c = co (\(DayOne far) -> runCo c far)

data ComponentsNel e v w ws where
  ComponentsOne :: Component w e v -> ComponentsNel e v w '[]
  (:::) :: Component w e v -> ComponentsNel e v  w' ws -> ComponentsNel e v w ( w' ': ws)

infixr 6 :::

-- Place N components next to one another
combineN
  :: forall w (ws :: [* -> *]) e v. Comonads (w ': ws)
  => (forall a. UI a e v -> UI a e v -> UI a e v)
  -> ComponentsNel e v w ws
  -> Component (DayNel w ws) e v
combineN _f (ComponentsOne (Component' c)) = Component' (DayOne c) & componentMapAction liftCoDayOne
-- This is when we switch back to x -> [x] -> x
-- Component' (DayOne (flip f [] <$> c)) & componentMapAction liftCoDayOne
combineN f ((Component' c1) ::: (cs :: ComponentsNel e v  w' ws1)) = Component' $ DayCons $ day (build <$> c1) recurse
  where
    (Component' recurse) = combineN f cs
    build :: (Comonads (w ': w1 ': ws1)) => UI (Co w1 ()) e v -> UI (Co (DayNel  w' ws1) ()) e v -> UI (Co (DayNel w1 ( w' ': ws1)) ()) e v
    build (UI render1) (UI render2) =
      f (UI (\send -> render1 $ \co -> send (liftLeft' co)))
        (UI (\send -> render2 $ \co -> send (peekRight' co)))

liftLeft' :: forall w w' ws a. (Functor w, Functor  w', Comonads ( w' ': ws)) => Co w a -> Co (DayNel w ( w' ': ws)) a
liftLeft' a = co (\(DayCons (Day w nel' f)) -> runCo a (fmap (`f` extract nel') w))

peekRight' :: forall w w' ws a. (Functor w, Comonad  w', Functor (DayNel w ws)) => Co (DayNel w ws) a -> Co (DayNel  w' (w ': ws)) a
peekRight' nel = co (\(DayCons (Day w' nel' f)) -> runCo nel (\war -> undefined)) -- todo

-- fmap (f (extract w')) nel'))
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

-- Choice
data Choice f g a = Choice Bool (f a) (g a)

instance (Functor f, Functor g) => Functor (Choice f g) where
  fmap op (Choice b fa ga) = Choice b (fmap op fa) (fmap op ga)

instance (Comonad f, Comonad g) => Comonad (Choice f g) where

  extend f (Choice b fa ga) =
    Choice b (extend (f <<< flip (Choice True) ga) fa)
      (extend (f <<< Choice False fa) ga)

  extract (Choice True fa _) = extract fa
  extract (Choice False _ ga) = extract ga

-- | Move to the true state.
moveTrue :: forall f g. (Comonad f, Functor g) => Co (Choice f g) ()
moveTrue = co (\(Choice _ fa _) -> extract fa ())

-- | Move to the false state.
moveFalse :: forall f g. (Comonad g, Functor f) => Co (Choice f g) ()
moveFalse = co (\(Choice _ _ ga) -> extract ga ())

-- | Lift an action to act on the true state.
liftTrue :: forall f g a. (Functor f, Functor g) => Co f a -> Co (Choice f g) a
liftTrue x = co (\(Choice _ fa _) -> runCo x fa)

-- | Lift an action to act on the right state.
liftFalse :: forall f g a. (Functor f, Functor g) => Co g a -> Co (Choice f g) a
liftFalse x = co (\(Choice _ _ ga) -> runCo x ga)

-- | Stack two components and show the front
stack
  :: forall w1 w2 e1 e2 v. Comonad w1
  => Comonad w2
  => Component w1 e1 v
  -> Component w2 e2 v
  -> Component (Choice w1 w2) (e1, e2) v
stack c1 c2 =
  Component' $ Choice True (unpack front) (unpack back)
  where
    front :: Component' w1 (Co (Choice w1 w2)) (e1, e2) v
    front = c1 & componentMapAction liftTrue & lmap fst
    back :: Component' w2 (Co (Choice w1 w2)) (e1, e2) v
    back = c2 & componentMapAction liftFalse & lmap snd
