{-# LANGUAGE KindSignatures, RankNTypes, NoImplicitPrelude, DeriveFunctor, GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings #-}

module Barbq.UI (
  pairStateStore,
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
  runRenderM) where

import Relude hiding ((<|>))
import Graphics.Vty
import Control.Monad.Co
import Control.Comonad (Comonad, extract, duplicate)
import Control.Comonad.Store
import Control.Monad.Writer (runWriter, Writer, tell, WriterT(..))
import Control.Comonad.Traced
import Data.Functor.Day

type Pairing f g = forall a b c. (a -> b -> c) -> f a -> g b -> c

pairCo :: Functor f => Pairing f (Co f)
pairCo f fa cofb = runCo cofb (f <$> fa)

pairSym :: forall f g. Pairing f g -> Pairing g f
pairSym pairing f ga fb = pairing (flip f) fb ga

pairDay :: forall f1 f2 g1 g2. Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Day f1 g1) (Day f2 g2)
pairDay p1 p2 f (Day f1 g1 g) (Day f2 g2 h) =
  let (x1, x2) = p1 (,) f1 f2 in
  let (y1, y2) = p2 (,) g1 g2 in
  f (g x1 y1) (h x2 y2)

newtype RenderM a = RenderM (StateT [RenderM ()] (ReaderT Vty IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Vty, MonadState [RenderM ()], MonadIO)

runRenderM :: RenderM a -> Vty -> IO a
runRenderM (RenderM st) vty = do
  (a, actions) <- runReaderT (runStateT st []) vty
  forM_ actions (`runRenderM` vty)
  return a

type Handler a = a -> RenderM ()

type UI a = Handler a -> RenderM ()

type Component' w m = w (UI (m ()))
type Component w = Component' w (Co w)

componentMapAction :: forall m1 m2 w.
                      Functor w => (m1 () -> m2 ()) ->
                      Component' w m1 ->
                      Component' w m2
componentMapAction f = fmap transformUi
  where
    transformUi :: UI (m1 ()) -> UI (m2 ())
    transformUi ui1 send = ui1 (send <<< f)

stateToCoStore :: forall f g a s. State s a -> Co (Store s) a
stateToCoStore state = co (story state)
  where
    story :: forall r. State s a -> Store s (a -> r) -> r
    story state store =
      let (render, s) = runStore store in
      let (a, s') = runState state s in
      render s' a

writerToCoTraced :: forall f g a s. Monoid s => Writer s a -> Co (Traced s) a
writerToCoTraced writer = co (tracer writer)
  where
    tracer :: forall r. Writer s a -> Traced s (a -> r) -> r
    tracer writer traced =
      let (a, s) = runWriter writer in
      let f = runTraced traced s in
      f a

pairIdentity :: Pairing Identity Identity
pairIdentity f fa gb = runIdentity (liftA2 f fa gb)

pairStateStore :: forall f g s. Pairing f g -> Pairing (StateT s f) (StoreT s g)
pairStateStore pairing f (StateT state) (StoreT gf s) =
  pairing (\(a, s1) f1 -> f a (f1 s1)) (state s) gf

pairWriterTraced :: forall f g s. Pairing f g -> Pairing (WriterT s f) (TracedT s g)
pairWriterTraced pairing f (WriterT writer) (TracedT gf) =
  pairing (\(a, w) f1 -> f a (f1 w)) writer gf

explore :: forall w m. Comonad w => Pairing m w -> Component' w m -> RenderM ()
explore pair space = extract space send
    where
      send :: m () -> RenderM ()
      send action = do
        let space' = (pair (const id) action <<< duplicate) space
        explore pair space'

exploreCo :: forall w. Comonad w => Component w -> RenderM ()
exploreCo = explore (pairSym pairCo)

combine :: forall w1 w2
         . Comonad w1
        => Comonad w2
        => (forall a. UI a -> UI a -> UI a)
        -> Component w1
        -> Component w2
        -> Component (Day w1 w2)
combine with w1 = day (build <$> w1) where
  build :: UI (Co w1 ()) -> UI (Co w2 ()) -> UI (Co (Day w1 w2) ())
  build render1 render2 = with (\send -> render1 $ \co -> send (liftLeft co))
                               (\send -> render2 $ \co -> send (liftRight co))

liftLeft :: forall w w' a. Functor w => Comonad w' => Co w a -> Co (Day w w') a
liftLeft a = co (\(Day w w' f) -> runCo a (fmap (`f` extract w') w))

liftRight :: forall w w' a. Functor w => Comonad w' => Co w a -> Co (Day w' w) a
liftRight a = co (\(Day w' w f) -> runCo a (fmap (f (extract w')) w))

newtype AddingInt = AddingInt Int

instance Semigroup AddingInt where
  (<>) (AddingInt i1) (AddingInt i2) = AddingInt $ i1 + i2
instance Monoid AddingInt where
  mempty = AddingInt 0

tracedExample :: Component' (Traced AddingInt) (Writer AddingInt)
tracedExample = traced render where
  render :: AddingInt -> UI (Writer AddingInt ())
  render (AddingInt count) send = do
    vty <- ask
    let line0 = text (defAttr ` withForeColor ` green) ("Traced " <> show count <> " line")
        line1 = string (defAttr ` withBackColor ` blue) "second line"
        img = line0 <|> line1
        pic = picForImage img
    liftIO $ update vty pic
    e <- liftIO $ nextEvent vty
    if count < 10 then
      modify ((:) $ send (tell (AddingInt 1)))
    else
      liftIO $ print "done"

storeExample :: Component' (Store Int) (State Int)
storeExample = store render 0 where
  render :: Int -> UI (State Int ())
  render count send = do
    vty <- ask
    let line0 = text (defAttr ` withForeColor ` green) ("Store " <> show count <> " line")
        line1 = string (defAttr ` withBackColor ` blue) "second line"
        img = line0 <|> line1
        pic = picForImage img
    liftIO $ update vty pic
    e <- liftIO $ nextEvent vty
    if count < 10 then
      modify ((:) $ send (modify (+1)))
    else
      liftIO $ print "done3"

combinedExample :: Component (Day (Store Int) (Traced AddingInt))
combinedExample = combine with store traced
  where
    with :: forall a. UI a -> UI a -> UI a
    with ui1 ui2 send = do
      ui1 send
      ui2 send

    traced :: Component (Traced AddingInt)
    traced = componentMapAction writerToCoTraced tracedExample

    store :: Component (Store Int)
    store = componentMapAction stateToCoStore storeExample



