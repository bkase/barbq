{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Barbq.UI.Runtime
  ( RenderM (..),
    runRenderM,
    combinedExample,
    explore,
    exploreCo
    )
where

import Barbq.UI.Framework
import Control.Comonad (Comonad, duplicate, extract)
import Control.Comonad.Store
import Control.Comonad.Traced
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Functor.Day
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Pipes ((>->), Consumer, await, runEffect)
import Pipes.Concurrent (Input, fromInput)
import Relude hiding ((<|>))

newtype RenderM a = RenderM (ReaderT V.Vty IO a)
  deriving (Functor, Applicative, Monad, MonadReader V.Vty, MonadIO)

runRenderM :: Consumer e RenderM () -> V.Vty -> Input e -> IO ()
runRenderM crm vty input = runReaderT m vty
  where
    m :: ReaderT V.Vty IO ()
    (RenderM m) = runEffect $ fromInput input >-> crm

explore :: forall w m e. Comonad w => Pairing m w -> Component' e V.Image w m -> Consumer (Maybe e) RenderM ()
explore pair space = do
  vty <- lift ask
  let (img, runner) = let { (UI ui) = extract space } in runWriter (ui send)
  let pic = V.picForImage img
  _bounds <- liftIO $ V.outputIface vty & V.displayBounds
  --liftIO $ print bounds
  liftIO $ V.update vty pic
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

exploreCo :: forall w e. Comonad w => Component e V.Image w -> Consumer (Maybe e) RenderM ()
exploreCo = explore (pairSym pairCo)

newtype AddingInt = AddingInt Int

instance Semigroup AddingInt where
  (<>) (AddingInt i1) (AddingInt i2) = AddingInt $ i1 + i2

instance Monoid AddingInt where
  mempty = AddingInt 0

tracedExample :: Component' e V.Image (Traced AddingInt) (Writer AddingInt)
tracedExample = traced render
  where
    render :: AddingInt -> UI e V.Image (Writer AddingInt ())
    render (AddingInt count) = UI $ \send -> do
      let line0 = V.text (V.defAttr `V.withForeColor` V.green) ("Traced " <> show count <> " line")
          line1 = V.string (V.defAttr `V.withBackColor` V.blue) "x"
          img = line0 <|> line1
      () <-
        tell $ const
          $ if count < 3
            then send (tell (AddingInt 1))
            else Endo id
      return img

storeExample :: Component' e V.Image (Store Int) (State Int)
storeExample = store render 0
  where
    render :: Int -> UI e V.Image (State Int ())
    render count = UI $ \send -> do
      let line0 = V.text (V.defAttr `V.withForeColor` V.green) ("Store " <> show count <> " line")
          line1 = V.string (V.defAttr `V.withBackColor` V.blue) "x"
          img = line0 <|> line1
      () <-
        tell $ const
          $ if count < 3
            then send (modify (+ 1))
            else Endo id
      return $ img & V.resizeWidth 20 & V.translateX 3

combinedExample :: forall e. Component e V.Image (Day (Store Int) (Traced AddingInt))
combinedExample = combine with store traced
  where
    with :: forall a. UI e V.Image a -> UI e V.Image a -> UI e V.Image a
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> pic2
    traced :: Component e V.Image (Traced AddingInt)
    traced = componentMapAction writerToCoTraced tracedExample
    store :: Component e V.Image (Store Int)
    store = componentMapAction stateToCoStore storeExample
