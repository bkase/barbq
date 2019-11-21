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
    runRenderM,
    realComponent,
    Pairing
    )
where

import Barbq.Types
import Control.Comonad (Comonad, duplicate, extract)
import Control.Comonad.Store
import Control.Comonad.Traced
import Control.Monad.Co
import Control.Monad.Writer (Writer, WriterT (..), mapWriter, runWriter, tell)
import Data.Functor.Day
import Data.Text.Lazy hiding (foldr, intersperse, take)
import Graphics.Vty hiding (Event, Input)
import Pipes ((>->), Consumer, Effect, Producer, await, runEffect, yield)
import Pipes.Concurrent (Input, fromInput)
import Relude hiding ((<|>), Text, filter)

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

newtype RenderM a = RenderM (ReaderT Vty IO a)
  deriving (Functor, Applicative, Monad, MonadReader Vty, MonadIO)

runRenderM :: Consumer e RenderM () -> Vty -> Input e -> IO ()
runRenderM crm vty input = runReaderT m vty
  where
    m :: ReaderT Vty IO ()
    (RenderM m) = runEffect $ fromInput input >-> crm

type SendResult s = Endo s

type Responder e s = e -> SendResult s

type Handler a s = a -> SendResult s

newtype UI e v a = UI (forall s. (Handler a s -> Writer (Responder e s) v))

type Component' e v w m = w (UI e v (m ()))

type Component e v w = Component' e v w (Co w)

componentMapView
  :: forall m w e v1 v2. Functor w
  => (v1 -> v2)
  -> Component' e v1 w m
  -> Component' e v2 w m
componentMapView f = fmap transformUi
  where
    transformUi :: UI e v1 (m ()) -> UI e v2 (m ())
    transformUi (UI ui) = UI $ fmap f . ui

componentMapAction
  :: forall m1 m2 w e v. Functor w
  => (m1 () -> m2 ())
  -> Component' e v w m1
  -> Component' e v w m2
componentMapAction f = fmap transformUi
  where
    transformUi :: UI e v (m1 ()) -> UI e v (m2 ())
    transformUi (UI ui) = UI $ \send -> ui (send <<< f)

componentPullbackEvent
  :: forall m w e1 e2 v. Functor w
  => (e2 -> e1)
  -> Component' e1 v w m
  -> Component' e2 v w m
componentPullbackEvent f = fmap transformUi
  where
    transformUi :: UI e1 v (m ()) -> UI e2 v (m ())
    transformUi (UI ui) = UI $ \send -> mapWriter writerMap (ui send)
    writerMap :: (v, Responder e1 s) -> (v, Responder e2 s)
    writerMap (img, r) = (img, r <<< f)

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

explore :: forall w m e. Comonad w => Pairing m w -> Component' e Image w m -> Consumer (Maybe e) RenderM ()
explore pair space = do
  vty <- lift ask
  let (img, runner) = let { (UI ui) = extract space } in runWriter (ui send)
  let pic = picForImage img
  bounds <- liftIO $ outputIface vty & displayBounds
  --liftIO $ print bounds
  liftIO $ update vty pic
  -- TODO: Keystrokes
  -- e <- liftIO $ nextEvent vty
  e <- await
  case e of
    Nothing -> return ()
    Just e -> explore pair (appEndo (runner e) space)
  where
    send :: forall v. m () -> SendResult (Component' e v w m)
    send action =
      Endo $ pair (const id) action <<< duplicate
    doEffect :: forall m'. Monad  m' => Effect  m' e ->  m' e
    doEffect = runEffect

exploreCo :: forall w e v. Comonad w => Component e Image w -> Consumer (Maybe e) RenderM ()
exploreCo = explore (pairSym pairCo)

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

newtype AddingInt = AddingInt Int

instance Semigroup AddingInt where
  (<>) (AddingInt i1) (AddingInt i2) = AddingInt $ i1 + i2

instance Monoid AddingInt where
  mempty = AddingInt 0

tracedExample :: Component' e Image (Traced AddingInt) (Writer AddingInt)
tracedExample = traced render
  where
    render :: AddingInt -> UI e Image (Writer AddingInt ())
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

storeExample :: Component' e Image (Store Int) (State Int)
storeExample = store render 0
  where
    render :: Int -> UI e Image (State Int ())
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

combinedExample :: forall e. Component e Image (Day (Store Int) (Traced AddingInt))
combinedExample = combine with store traced
  where
    with :: forall a. UI e Image a -> UI e Image a -> UI e Image a
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> pic2
    traced :: Component e Image (Traced AddingInt)
    traced = componentMapAction writerToCoTraced tracedExample
    store :: Component e Image (Store Int)
    store = componentMapAction stateToCoStore storeExample

pureProvidedComponent :: forall e v. e -> Reader e v -> Component' e v (Store e) (State e)
pureProvidedComponent z draw = store render z
  where
    render :: e -> UI e v (State e ())
    render e = UI $ \send -> do
      let v = runReader draw e
      () <- tell $ \e -> send (put e)
      return v

type PureBarbqComponent e = Component' e Image (Store e) (State e)

volumeComponent :: PureBarbqComponent Int
volumeComponent = pureProvidedComponent 0 $ do
  volume <- ask
  return $ text defAttr $ emoji volume <> show volume
  where
    emoji 0 = "\xf466  "
    emoji i = "\xf485  "

tabsComponent :: PureBarbqComponent (Maybe PointedFinSet)
tabsComponent = pureProvidedComponent Nothing $ do
  tabs <- ask
  pure $ draw tabs
  where
    build :: Int -> Int -> [(Int, Attr)]
    build i point = (i, if point == i then defAttr `withBackColor` blue else defAttr) : build (i + 1) point
    render :: (Int, Attr) -> Image
    render (i, attr) = text attr $ " " <> (show i) <> " "
    draw :: Maybe PointedFinSet -> Image
    draw Nothing = text defAttr ""
    draw (Just tabs) =
      let { list = take (maxSet tabs) (build 1 $ point tabs) }
       in list & fmap render
            -- & intersperse (text defAttr " ")
            & foldr (\x acc -> x <|> acc) (text defAttr "")

wifiComponent :: PureBarbqComponent (Maybe Text)
wifiComponent = pureProvidedComponent Nothing $ do
  ssid <- ask
  return $ text defAttr $ "\xf012  " <> emoji ssid
  where
    emoji Nothing = "Unknown"
    emoji (Just name) = filter (/= '\n') name

-- TODO: How to typelevel fold over '[A, B, C] for less boiler platyness
type Day3 f g h = Day f (Day g h)

realComponent :: Component (Maybe PointedFinSet, Int, Maybe Text) Image (Day3 (Store (Maybe PointedFinSet)) (Store Int) (Store (Maybe Text)))
realComponent = combine with tabs (combine with volume wifi)
  where
    with :: forall e a. UI e Image a -> UI e Image a -> UI e Image a
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> string defAttr "   " <|> pic2
    tabs :: forall a b. Component (Maybe PointedFinSet, a, b) Image (Store (Maybe PointedFinSet))
    tabs =
      tabsComponent & componentMapAction stateToCoStore
        & componentPullbackEvent (\(x, a, b) -> x)
    volume :: forall a b. Component (a, Int, b) Image (Store Int)
    volume =
      volumeComponent & componentMapAction stateToCoStore
        & componentPullbackEvent (\(a, x, b) -> x)
    wifi :: forall a b. Component (a, b, Maybe Text) Image (Store (Maybe Text))
    wifi =
      wifiComponent & componentMapAction stateToCoStore
        & componentPullbackEvent (\(a, b, x) -> x)
