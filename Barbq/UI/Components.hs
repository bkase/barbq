{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barbq.UI.Components
  ( realComponent
    )
where

import Barbq.Types
import Barbq.UI.Framework
import Control.Comonad.Store (Store, store)
import Control.Monad.Writer
import Data.Functor.Day
import Data.Profunctor
import Data.Text.Lazy (Text, filter)
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Relude hiding ((<|>), Text, filter)

pureProvidedComponent :: forall e v. e -> Reader e v -> Component' (Store e) (State e) e v
pureProvidedComponent z draw = Component' $ store render z
  where
    render :: e -> UI (State e ()) e v
    render e = UI $ \send -> do
      let v = runReader draw e
      () <- tell $ \e -> send (put e)
      return v

type PureBarbqComponent e = Component' (Store e) (State e) e V.Image

volumeComponent :: PureBarbqComponent Int
volumeComponent = pureProvidedComponent 0 $ do
  volume <- ask
  return $ V.text V.defAttr $ emoji volume <> show volume
  where
    emoji 0 = "\xf466  "
    emoji _ = "\xf485  "

tabsComponent :: PureBarbqComponent (Maybe PointedFinSet)
tabsComponent = pureProvidedComponent Nothing $ draw <$> ask
  where
    build :: Int -> Int -> [(Int, V.Attr)]
    build i point = (i, if point == i then V.defAttr `V.withBackColor` V.blue else V.defAttr) : build (i + 1) point
    render :: (Int, V.Attr) -> V.Image
    render (i, attr) = V.text attr $ " " <> show i <> " "
    draw :: Maybe PointedFinSet -> V.Image
    draw Nothing = V.text V.defAttr ""
    draw (Just tabs) =
      let list :: [(Int, V.Attr)] = take (maxSet tabs) (build 1 $ point tabs)
          list' :: [V.Image] = list & fmap render
       in list' & foldr (<|>) (V.text V.defAttr "")

wifiComponent :: PureBarbqComponent (Maybe Text)
wifiComponent = pureProvidedComponent Nothing $ do
  ssid <- ask
  return $ V.text V.defAttr $ "\xf012  " <> emoji ssid
  where
    emoji Nothing = "Unknown"
    emoji (Just name) = filter (/= '\n') name

-- TODO: How to typelevel fold over '[A, B, C] for less boiler platyness
type Day3 f g h = Day f (Day g h)

realComponent :: Component (Day3 (Store (Maybe PointedFinSet)) (Store Int) (Store (Maybe Text))) (Maybe PointedFinSet, Int, Maybe Text) V.Image
realComponent = combine with tabs (combine with volume wifi)
  where
    with :: forall e a. UI a e V.Image -> UI a e V.Image -> UI a e V.Image
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> V.string V.defAttr "   " <|> pic2
    tabs :: forall a b. Component (Store (Maybe PointedFinSet)) (Maybe PointedFinSet, a, b) V.Image
    tabs =
      tabsComponent & componentMapAction stateToCoStore
        & lmap (\(x, _, _) -> x)
    volume :: forall a b. Component (Store Int) (a, Int, b) V.Image
    volume =
      volumeComponent & componentMapAction stateToCoStore
        & lmap (\(_, x, _) -> x)
    wifi :: forall a b. Component (Store (Maybe Text)) (a, b, Maybe Text) V.Image
    wifi =
      wifiComponent & componentMapAction stateToCoStore
        & lmap (\(_, _, x) -> x)
