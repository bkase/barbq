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
import Control.Monad.Writer (tell)
import Data.Functor.Day
import Data.Profunctor
import Data.Semigroup (Last (..))
import Data.Text.Lazy (Text, filter, length, replicate, splitAt)
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Relude hiding ((<|>), Last, Text, filter, length, replicate, splitAt)

pureProvidedComponent' :: forall e v. e -> Reader e v -> Component' (Store e) (State e) e v
pureProvidedComponent' z draw = Component' $ store render z
  where
    render :: e -> UI (State e ()) e v
    render e = UI $ \send -> do
      let v = runReader draw e
      () <- tell $ \e -> send (put e)
      return v

pureProvidedComponent :: forall e v. Monoid e => Reader e v -> Component' (Store e) (State e) e v
pureProvidedComponent = pureProvidedComponent' mempty

type PureBarbqComponent e = Component' (Store e) (State e) e V.Image

scrollyComponent :: PureBarbqComponent ScrollyInput
scrollyComponent = pureProvidedComponent $ do
  (Last' content, Sum tick', width') <- ask
  case width' of
    Nothing -> pure $ V.text V.defAttr ""
    Just (Last width') -> do
      let (tick, width) = (fromIntegral tick', fromIntegral width')
      let startPos = tick `mod` width
      let len = length content
      if startPos + len > width
        then do
          let (startHalf, endHalf) = splitAt (len - (startPos + len - width)) content
          pure $ V.text V.defAttr $ endHalf <> replicate (fromIntegral (width - len)) " " <> startHalf
        else do
          let (pre, post) = (startPos, width - startPos - len)
          pure $ V.text V.defAttr $ replicate (fromIntegral pre) " " <> content <> replicate (fromIntegral post) " "

volumeComponent :: PureBarbqComponent (Last' (Sum Int))
volumeComponent = pureProvidedComponent $ do
  (Last' (Sum volume)) <- ask
  return $ V.text V.defAttr $ emoji volume <> show volume
  where
    emoji 0 = "\xf466  "
    emoji _ = "\xf485  "

tabsComponent :: PureBarbqComponent (Maybe (Last PointedFinSet))
tabsComponent = pureProvidedComponent $ draw <$> ask
  where
    build :: Int -> Int -> [(Int, V.Attr)]
    build i point = (i, if point == i then V.defAttr `V.withBackColor` V.blue else V.defAttr) : build (i + 1) point
    render :: (Int, V.Attr) -> V.Image
    render (i, attr) = V.text attr $ " " <> name i <> " "
    name :: Int -> Text
    name 1 = "\xf268 "
    name 2 = "\xf0e6 "
    name 3 = "\xf121 "
    name 4 = "\xf07c "
    name 5 = "\xf001 "
    name 6 = "\xf0ae "
    name i = show i
    draw :: Maybe (Last PointedFinSet) -> V.Image
    draw Nothing = V.text V.defAttr ""
    draw (Just (Last tabs)) =
      let list :: [(Int, V.Attr)] = take (maxSet tabs) (build 1 $ point tabs)
          list' :: [V.Image] = list & fmap render
       in list' & foldr (<|>) (V.text V.defAttr "")

wifiComponent :: PureBarbqComponent (Last' Text)
wifiComponent = pureProvidedComponent $ do
  (Last' ssid) <- ask
  return $ V.text V.defAttr $ "\xf1eb  " <> emoji ssid
  where
    emoji name
      | name == "" = "Unknown"
      | otherwise = filter (/= '\n') name

-- TODO: How to typelevel fold over '[A, B, C] for less boiler platyness
type Day3 f g h = Day f (Day g h)

type Day4 f g h i = Day f (Day3 g h i)

type A = Maybe (Last PointedFinSet)

type B = Last' (Sum Int)

type C = Last' Text

type D = ScrollyInput

realComponent :: Component (Day4 (Store A) (Store B) (Store C) (Store D)) (A, B, C, D) V.Image
realComponent = combine with tabs (combine with volume (combine with wifi scrolly))
  where
    with :: forall e a. UI a e V.Image -> UI a e V.Image -> UI a e V.Image
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> V.string V.defAttr "   " <|> pic2
    tabs :: forall b c d. Component (Store A) (A, b, c, d) V.Image
    tabs =
      tabsComponent & componentMapAction stateToCoStore
        & lmap (\(a, _, _, _) -> a)
    volume :: forall a c d. Component (Store B) (a, B, c, d) V.Image
    volume =
      volumeComponent & componentMapAction stateToCoStore
        & lmap (\(_, b, _, _) -> b)
    wifi :: forall a b d. Component (Store C) (a, b, C, d) V.Image
    wifi =
      wifiComponent & componentMapAction stateToCoStore
        & lmap (\(_, _, c, _) -> c)
    scrolly :: forall a b c. Component (Store D) (a, b, c, D) V.Image
    scrolly =
      scrollyComponent & componentMapAction stateToCoStore
        & lmap (\(_, _, _, d) -> d)
