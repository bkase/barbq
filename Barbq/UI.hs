{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barbq.UI
  ( pairStateStore,
    pairWriterTraced,
    pairIdentity,
    explore,
    storeExample,
    tracedExample,
    combinedExample,
    exploreCo,
    pairSym,
    pairCo,
    pairDay,
    runRenderM
    )
where

import Control.Comonad (Comonad, duplicate, extract)
import Control.Comonad.Store
import Control.Comonad.Traced
import Control.Monad.Co
import Control.Monad.Writer (Writer, WriterT (..), mapWriter, runWriter, tell)
import Data.Functor.Day
import Graphics.Vty hiding (Event, Input)
import Pipes ((>->), Consumer, Effect, Producer, await, runEffect, yield)
import Pipes.Concurrent (Input, fromInput)
import Relude hiding ((<|>))

type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

pairCo :: Functor f => Pairing f (Co f)
pairCo f fa cofb = runCo cofb (f <$> fa)

pairSym :: forall f g. Pairing f g -> Pairing g f
pairSym pairing f ga fb = pairing (flip f) fb ga

pairDay :: forall f1 f2 g1 g2. Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Day f1 g1) (Day f2 g2)
pairDay p1 p2 f (Day f1 g1 g) (Day f2 g2 h) =
  let (x1, x2) = p1 (,) f1 f2
   in let (y1, y2) = p2 (,) g1 g2
       in f (g x1 y1) (h x2 y2)

type SendResult s = Endo s

type Responder e s = e -> SendResult s

newtype RenderM a = RenderM (ReaderT Vty IO a)
  deriving (Functor, Applicative, Monad, MonadReader Vty, MonadIO)

runRenderM :: Consumer e RenderM () -> Vty -> Input e -> IO ()
runRenderM crm vty input = runReaderT m vty
  where
    m :: ReaderT Vty IO ()
    (RenderM m) = runEffect $ fromInput input >-> crm

type Handler a s = a -> SendResult s

-- a picture and function that reacts to events
newtype UI e a = UI (forall s. (Handler a s -> WriterT (Responder e s) Identity Image))

type Component' e w m = w (UI e (m ()))

type Component e w = Component' e w (Co w)

componentMapAction
  :: forall m1 m2 w e. Functor w
  => (m1 () -> m2 ())
  -> Component' e w m1
  -> Component' e w m2
componentMapAction f = fmap transformUi
  where
    transformUi :: UI e (m1 ()) -> UI e (m2 ())
    transformUi (UI ui) = UI $ \send -> ui (send <<< f)

componentPullbackEvent
  :: forall m w e1 e2. Functor w
  => (e2 -> e1)
  -> Component' e1 w m
  -> Component' e2 w m
componentPullbackEvent f = fmap transformEvent
  where
    transformEvent :: UI e1 (m ()) -> UI e2 (m ())
    transformEvent (UI ui) = UI $ \send -> mapWriter writerMap (ui send)
    writerMap :: (Image, Responder e1 s) -> (Image, Responder e2 s)
    writerMap (img, r) = (img, \e2 -> r (f e2))

stateToCoStore :: forall f g a s. State s a -> Co (Store s) a
stateToCoStore state = co (story state)
  where
    story :: forall r. State s a -> Store s (a -> r) -> r
    story state store =
      let { (render, s) = runStore store }
       in let { (a, s') = runState state s }
           in render s' a

writerToCoTraced :: forall f g a s. Monoid s => Writer s a -> Co (Traced s) a
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

explore :: forall w m e. Comonad w => Pairing m w -> Component' e w m -> Consumer (Maybe e) RenderM ()
explore pair space = do
  vty <- lift ask
  let (img, runner) = let { (UI ui) = extract space } in runWriter (ui send)
  let pic = picForImage img
  bounds <- liftIO $ outputIface vty & displayBounds
  liftIO $ print bounds
  liftIO $ update vty pic
  -- TODO: Keystrokes
  -- e <- liftIO $ nextEvent vty
  e <- await
  case e of
    Nothing -> return ()
    Just e -> explore pair (appEndo (runner e) space)
  where
    send :: m () -> SendResult (Component' e w m)
    send action =
      Endo $ pair (const id) action <<< duplicate
    doEffect :: forall m'. Monad  m' => Effect  m' e ->  m' e
    doEffect = runEffect

exploreCo :: forall w e. Comonad w => Component e w -> Consumer (Maybe e) RenderM ()
exploreCo = explore (pairSym pairCo)

combine
  :: forall w1 w2 e. Comonad w1
  => Comonad w2
  => (forall a. UI e a -> UI e a -> UI e a)
  -> Component e w1
  -> Component e w2
  -> Component e (Day w1 w2)
combine with w1 = day (build <$> w1)
  where
    build :: UI e (Co w1 ()) -> UI e (Co w2 ()) -> UI e (Co (Day w1 w2) ())
    build (UI render1) (UI render2) =
      with (UI (\send -> render1 $ \co -> send (liftLeft co)))
        (UI (\send -> render2 $ \co -> send (liftRight co)))

liftLeft :: forall w w' a. Functor w => Comonad  w' => Co w a -> Co (Day w  w') a
liftLeft a = co (\(Day w w' f) -> runCo a (fmap (`f` extract w') w))

liftRight :: forall w w' a. Functor w => Comonad  w' => Co w a -> Co (Day  w' w) a
liftRight a = co (\(Day w' w f) -> runCo a (fmap (f (extract w')) w))

newtype AddingInt = AddingInt Int

instance Semigroup AddingInt where
  (<>) (AddingInt i1) (AddingInt i2) = AddingInt $ i1 + i2

instance Monoid AddingInt where
  mempty = AddingInt 0

tracedExample :: Component' e (Traced AddingInt) (Writer AddingInt)
tracedExample = traced render
  where
    render :: AddingInt -> UI e (Writer AddingInt ())
    render (AddingInt count) = UI $ \send -> do
      let line0 = text (defAttr `withForeColor` green) ("Traced " <> show count <> " line")
          line1 = string (defAttr `withBackColor` blue) "x"
          img = line0 <|> line1
      () <-
        tell $ const
          $ if count < 3
            then send (tell (AddingInt 1))
            else Endo id
      return img

storeExample :: Component' e (Store Int) (State Int)
storeExample = store render 0
  where
    render :: Int -> UI e (State Int ())
    render count = UI $ \send -> do
      let line0 = text (defAttr `withForeColor` green) ("Store " <> show count <> " line")
          line1 = string (defAttr `withBackColor` blue) "x"
          img = line0 <|> line1
      () <-
        tell $ const
          $ if count < 3
            then send (modify (+ 1))
            else Endo id
      return $ img & resizeWidth 20 & translateX 3

combinedExample :: forall e. Component e (Day (Store Int) (Traced AddingInt))
combinedExample = combine with store traced
  where
    with :: forall a. UI e a -> UI e a -> UI e a
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> pic2
    traced :: Component e (Traced AddingInt)
    traced = componentMapAction writerToCoTraced tracedExample
    store :: Component e (Store Int)
    store = componentMapAction stateToCoStore storeExample
