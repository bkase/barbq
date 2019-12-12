{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Barbq.UI.Components
  ( realComponent
    )
where

import Barbq.Types
import Barbq.UI.Framework
import Control.Comonad (Comonad)
import Control.Comonad.Store (Store, store)
import Control.Lens ((^.), view)
import Control.Monad.Co
import Control.Monad.Writer (tell)
import Control.Newtype
import Data.Functor.Day
import Data.Profunctor hiding (Choice)
import Data.Text.Lazy (Text, filter, length, replicate, splitAt)
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Relude hiding ((<|>), Text, filter, length, replicate, splitAt)
import qualified Relude as R

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

type StaticCo = Identity

static :: V.Image -> Component StaticCo () V.Image
static image = Component' $ Identity render
  where
    render :: UI (Co Identity ()) () V.Image
    render = UI $ \_ -> return image

-- Flexish layout
liftLayout2
  :: forall a e i. (i -> V.Image -> V.Image -> V.Image)
  -> i
  -> UI a e V.Image
  -> UI a e V.Image
  -> UI a e V.Image
liftLayout2 layout parentWidth (UI ui1) (UI ui2) = UI $ \send -> do
  left <- ui1 send
  right <- ui2 send
  return $ layout parentWidth left right

liftLayoutN
  :: forall a e i. (i -> [V.Image] -> V.Image)
  -> i
  -> [UI a e V.Image]
  -> UI a e V.Image
liftLayoutN layout parentWidth uis = UI $ \send -> do
  imgs <- mapM (\(UI ui) -> ui send) uis
  return $ layout parentWidth imgs

spaces :: Int64 -> Text
spaces i = replicate i " "

layoutSpaceBetween :: forall a e i. Integral i => i -> UI a e V.Image -> UI a e V.Image -> UI a e V.Image
layoutSpaceBetween = liftLayout2 layout
  where
    layout :: i -> V.Image -> V.Image -> V.Image
    layout parentWidth left right =
      let w1 = fromIntegral $ V.imageWidth left
          w2 = fromIntegral $ V.imageWidth right
          extraSpace w1 w2 = spaces $ fromIntegral (parentWidth - w1 - w2)
       in left <|> V.text V.defAttr (extraSpace w1 w2) <|> right

layoutSpaceAround :: forall a e i. Integral i => i -> [UI a e V.Image] -> UI a e V.Image
layoutSpaceAround = liftLayoutN layout
  where
    layout :: i -> [V.Image] -> V.Image
    layout parentWidth children =
      let ws :: [Float] = fromIntegral . V.imageWidth <$> children
          pw :: Float = fromIntegral parentWidth
          sectionw = pw / fromIntegral (R.length ws)
          extraws = (\w -> (sectionw - w) / 2) <$> ws
          space i = V.text V.defAttr $ spaces i
          spaced = zipWith (\extraw child -> space (round extraw) <|> child <|> space (round extraw)) extraws children
       in spaced & foldr (<|>) mempty

two :: forall a b. ([a] -> b) -> (a -> a -> b)
two f x y = f [x, y]

-- Content
type Schema' = Maybe Schema

type ScrollyCo = Store (Maybe ScrollyInput)

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

type TabsCo = Store (Maybe PointedFinSet')

tabsComponent :: PureBarbqComponent PointedFinSet'
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

type VolumeCo = Store (Maybe Volume)

volumeComponent :: PureBarbqComponent Volume
volumeComponent = pureProvidedComponent $ do
  v :: Volume <- ask
  return $ V.text V.defAttr $ emoji (v ^. volumeIsMuted) <> show (v ^. volumeAmount)
  where
    emoji True = "\xf466  "
    emoji False = "\xf485  "

knownWifiComponent :: Component ScrollyCo (Maybe Wifi') V.Image
knownWifiComponent =
  scrollyComponent & dimap pullback wifiPrefix
  where
    pullback :: Maybe Wifi' -> Maybe ScrollyInput
    pullback mc = mc & fmap (fmap $ \(content, tick) -> (content, tick, 15, 5)) & join
    wifiPrefix :: V.Image -> V.Image
    wifiPrefix img = if V.imageWidth img == 0 then img else V.text V.defAttr "\xf1eb  " <|> img

unknownWifiComponent :: Component StaticCo () V.Image
unknownWifiComponent = static $ V.text V.defAttr "\xf127  Disconnected"

type WifiCo = Choice ScrollyCo StaticCo

wifiComponent :: Component WifiCo (Maybe Wifi') V.Image
wifiComponent =
  stack knownWifiComponent unknownWifiComponent & lmap (,())
    & over Component' (fmap transformUi)
  where
    transformUi :: forall w1 w2 v. (Comonad w1, Comonad w2) => UI (Co (Choice w1 w2) ()) (Maybe Wifi') v -> UI (Co (Choice w1 w2) ()) (Maybe Wifi') v
    transformUi (UI ui) = UI $ \send -> do
      () <- tell $ move send
      ui send
    move send Nothing = send moveTrue
    move send (Just Nothing) = send moveFalse
    move send (Just (Just _)) = send moveTrue

type CalendarCo = Store (Maybe Calendar)

calendarComponent :: PureBarbqComponent Calendar
calendarComponent = pureProvidedComponent $ do
  cal :: Calendar <- ask
  return $ draw $ filter (/= '\n') <$> cal ^. calendarDate
  where
    draw :: Maybe Text -> V.Image
    draw Nothing = mempty
    draw (Just text) = V.text V.defAttr $ "\xf133  " <> text

type family DayN f fs where
  DayN f '[] = f
  DayN f (g ': hs) = Day f (DayN g hs)

realComponent :: Int -> Component (DayN TabsCo '[VolumeCo, WifiCo, CalendarCo]) Schema' V.Image
realComponent parentWidth = combine (layoutSpaceBetween parentWidth) tabs (combine (two $ layoutSpaceAround $ parentWidth `div` 2) volume (combine (layoutSpaceBetween $ parentWidth `div` 4) wifi calendar))
  where
    tabs :: Component TabsCo Schema' V.Image
    tabs =
      tabsComponent & lmap (fmap $ view schemaTabs)
    volume :: Component VolumeCo Schema' V.Image
    volume =
      volumeComponent & lmap (fmap $ view schemaVolume)
    wifi :: Component WifiCo Schema' V.Image
    wifi =
      wifiComponent & lmap (fmap viewSchemaWifi')
    calendar :: Component CalendarCo Schema' V.Image
    calendar =
      calendarComponent & lmap (fmap $ view schemaCalendar)
