{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Barbq.UI.Components
  ( realComponent
    )
where

import Barbq.Types
import Barbq.UI.Framework
import Control.Comonad (Comonad)
import Control.Comonad.Store (Store, store)
import Control.Monad.Co
import Control.Monad.Writer (tell)
import Control.Newtype
import Data.Functor.Day
import Data.Profunctor hiding (Choice)
import Data.Text.Lazy (Text, length, replicate, splitAt)
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Relude hiding ((<|>), Text, filter, length, replicate, splitAt)

type PureBarbqComponent e = Component (Store (Maybe e)) (Maybe e) V.Image

pureProvidedComponent :: forall e. Reader e V.Image -> PureBarbqComponent e
pureProvidedComponent draw = Component' $ store render Nothing
  where
    render :: Maybe e -> UI (Co (Store (Maybe e)) ()) (Maybe e) V.Image
    render e = UI $ \send -> do
      () <- tell $ \e -> send (unpack' $ put e)
      case e of
        Nothing -> pure $ V.text V.defAttr ""
        Just e -> do
          let v = runReader draw e
          return v
    unpack' = unpack @(Co' (Store (Maybe e)) ())

static :: V.Image -> Component Identity () V.Image
static image = Component' $ Identity render
  where
    render :: UI (Co Identity ()) () V.Image
    render = UI $ \_ -> return image

type X a = Maybe a

type A = Maybe PointedFinSet

type B = Int

type C = Maybe (Text, Sum Int)

-- youtube "\xf167 \xf04b "
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
  pure $ V.text V.defAttr viewport

tabsComponent :: PureBarbqComponent A
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

volumeComponent :: PureBarbqComponent B
volumeComponent = pureProvidedComponent $ do
  volume <- ask
  return $ V.text V.defAttr $ emoji volume <> show volume
  where
    emoji 0 = "\xf466  "
    emoji _ = "\xf485  "

type E = (Text, Sum Int, Int, Int)

knownWifiComponent :: Component (Store (Maybe E)) (Maybe C) V.Image
knownWifiComponent =
  scrollyComponent & dimap pullback wifiPrefix
  where
    pullback :: Maybe C -> Maybe E
    pullback mc = mc & fmap (fmap $ \(content, tick) -> (content, tick, 15, 5)) & join
    wifiPrefix :: V.Image -> V.Image
    wifiPrefix img = if V.imageWidth img == 0 then img else V.text V.defAttr "\xf1eb  " <|> img

unknownWifiComponent :: Component Identity () V.Image
unknownWifiComponent = static $ V.text V.defAttr "\xf127  Disconnected"

wifiComponent :: Component (Choice (Store (Maybe E)) Identity) (Maybe C) V.Image
wifiComponent =
  stack knownWifiComponent unknownWifiComponent & lmap (,())
    & over Component' (fmap transformUi)
  where
    transformUi :: forall w1 w2 v. (Comonad w1, Comonad w2) => UI (Co (Choice w1 w2) ()) (Maybe C) v -> UI (Co (Choice w1 w2) ()) (Maybe C) v
    transformUi (UI ui) = UI $ \send -> do
      () <- tell $ move send
      ui send
    move send Nothing = send moveTrue
    move send (Just Nothing) = send moveFalse
    move send (Just (Just _)) = send moveTrue

-- TODO: How to typelevel fold over '[A, B, C] for less boiler platyness
type Day3 f g h = Day f (Day g h)

-- type Day4 f g h i = Day f (Day3 g h i)
fst' :: forall a b c. (a, b, c) -> a
fst' (a, _, _) = a

snd' :: forall a b c. (a, b, c) -> b
snd' (_, b, _) = b

third' :: forall a b c. (a, b, c) -> c
third' (_, _, c) = c

-- fourth' :: forall a b c d. (a, b, c, d) -> d
-- fourth' (_, _, _, d) = d
realComponent :: Component (Day3 (Store (X A)) (Store (X B)) (Choice (Store (X ScrollyInput)) Identity)) (X (A, B, C)) V.Image
realComponent = combine with tabs (combine with volume wifi)
  where
    with :: forall e a. UI a e V.Image -> UI a e V.Image -> UI a e V.Image
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      return $ pic1 <|> V.string V.defAttr "   " <|> pic2
    tabs :: forall b c. Component (Store (X A)) (X (A, b, c)) V.Image
    tabs =
      tabsComponent & lmap (fmap fst')
    volume :: forall a c. Component (Store (X B)) (X (a, B, c)) V.Image
    volume =
      volumeComponent & lmap (fmap snd')
    wifi :: forall a b. Component (Choice (Store (X ScrollyInput)) Identity) (X (a, b, C)) V.Image
    wifi =
      wifiComponent & lmap (fmap third')
