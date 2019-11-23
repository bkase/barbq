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
import Data.Text.Lazy (Text, filter)
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Relude hiding ((<|>), Text, filter)

pureProvidedComponent :: forall e v. e -> Reader e v -> Component' e v (Store e) (State e)
pureProvidedComponent z draw = store render z
  where
    render :: e -> UI e v (State e ())
    render e = UI $ \send -> do
      let v = runReader draw e
      () <- tell $ \e -> send (put e)
      return v

type PureBarbqComponent e = Component' e V.Image (Store e) (State e)

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

realComponent :: Component (Maybe PointedFinSet, Int, Maybe Text) V.Image (Day3 (Store (Maybe PointedFinSet)) (Store Int) (Store (Maybe Text)))
realComponent = combine with tabs (combine with volume wifi)
  where
    with :: forall e a. UI e V.Image a -> UI e V.Image a -> UI e V.Image a
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> V.string V.defAttr "   " <|> pic2
    tabs :: forall a b. Component (Maybe PointedFinSet, a, b) V.Image (Store (Maybe PointedFinSet))
    tabs =
      tabsComponent & componentMapAction stateToCoStore
        & componentPullbackEvent (\(x, _, _) -> x)
    volume :: forall a b. Component (a, Int, b) V.Image (Store Int)
    volume =
      volumeComponent & componentMapAction stateToCoStore
        & componentPullbackEvent (\(_, x, _) -> x)
    wifi :: forall a b. Component (a, b, Maybe Text) V.Image (Store (Maybe Text))
    wifi =
      wifiComponent & componentMapAction stateToCoStore
        & componentPullbackEvent (\(_, _, x) -> x)
