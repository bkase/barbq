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
import Data.Text.Lazy (Text, filter, length, replicate, splitAt)
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Relude hiding ((<|>), Text, filter, length, replicate, splitAt)

type PureBarbqComponent e = Component' (Store (Maybe e)) (State (Maybe e)) (Maybe e) V.Image

pureProvidedComponent :: forall e. Reader e V.Image -> PureBarbqComponent e
pureProvidedComponent draw = Component' $ store render Nothing
  where
    render :: Maybe e -> UI (State (Maybe e) ()) (Maybe e) V.Image
    render e = UI $ \send -> do
      () <- tell $ \e -> send (put e)
      case e of
        Nothing -> pure $ V.text V.defAttr ""
        Just e -> do
          let v = runReader draw e
          return v

scrollyComponent :: PureBarbqComponent ScrollyInput
scrollyComponent = pureProvidedComponent $ do
  (content, Sum tick', width', f') <- ask
  let (tick, width, f) = (fromIntegral tick', fromIntegral width', fromIntegral f')
  let extraSpaces = max (width - length content) 3
  let paddedContent = content <> replicate (fromIntegral extraSpaces) " "
  let c = length paddedContent
  let tick' = tick `mod` (c + f)
  let tick'' = max (tick' - f) 0
  let contentDoubled = paddedContent <> paddedContent
  let (_, viewportExtended) = splitAt tick'' contentDoubled
  let (viewport, _) = splitAt width viewportExtended
  pure $ V.text V.defAttr $ "\xf167 \xf04b " <> viewport

volumeComponent :: PureBarbqComponent Int
volumeComponent = pureProvidedComponent $ do
  volume <- ask
  return $ V.text V.defAttr $ emoji volume <> show volume
  where
    emoji 0 = "\xf466  "
    emoji _ = "\xf485  "

tabsComponent :: PureBarbqComponent (Maybe PointedFinSet)
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
    draw :: Maybe PointedFinSet -> V.Image
    draw Nothing = V.text V.defAttr ""
    draw (Just tabs) =
      let list :: [(Int, V.Attr)] = take (maxSet tabs) (build 1 $ point tabs)
          list' :: [V.Image] = list & fmap render
       in list' & foldr (<|>) (V.text V.defAttr "")

wifiComponent :: PureBarbqComponent Text
wifiComponent = pureProvidedComponent $ do
  ssid <- ask
  return $ V.text V.defAttr $ "\xf1eb  " <> emoji ssid
  where
    emoji name
      | name == "" = "Unknown"
      | otherwise = filter (/= '\n') name

-- TODO: How to typelevel fold over '[A, B, C] for less boiler platyness
type Day3 f g h = Day f (Day g h)

type Day4 f g h i = Day f (Day3 g h i)

type X a = Maybe a

type A = Maybe PointedFinSet

type B = Int

type C = Text

type D = ScrollyInput

fst' :: forall a b c d. (a, b, c, d) -> a
fst' (a, _, _, _) = a

snd' :: forall a b c d. (a, b, c, d) -> b
snd' (_, b, _, _) = b

third' :: forall a b c d. (a, b, c, d) -> c
third' (_, _, c, _) = c

fourth' :: forall a b c d. (a, b, c, d) -> d
fourth' (_, _, _, d) = d

realComponent :: Component (Day4 (Store (X A)) (Store (X B)) (Store (X C)) (Store (X D))) (X (A, B, C, D)) V.Image
realComponent = combine with tabs (combine with volume (combine with wifi scrolly))
  where
    with :: forall e a. UI a e V.Image -> UI a e V.Image -> UI a e V.Image
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> V.string V.defAttr "   " <|> pic2
    tabs :: forall b c d. Component (Store (X A)) (X (A, b, c, d)) V.Image
    tabs =
      tabsComponent & componentMapAction stateToCoStore
        & lmap (fmap fst')
    volume :: forall a c d. Component (Store (X B)) (X (a, B, c, d)) V.Image
    volume =
      volumeComponent & componentMapAction stateToCoStore
        & lmap (fmap snd')
    wifi :: forall a b d. Component (Store (X C)) (X (a, b, C, d)) V.Image
    wifi =
      wifiComponent & componentMapAction stateToCoStore
        & lmap (fmap third')
    scrolly :: forall a b c. Component (Store (X D)) (X (a, b, c, D)) V.Image
    scrolly =
      scrollyComponent & componentMapAction stateToCoStore
        & lmap (fmap fourth')
