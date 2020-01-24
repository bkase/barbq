{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Comonad.Traced hiding (trace)
import Control.Monad.Co
import Control.Monad.State.Class
import Control.Monad.Writer (runWriter, tell)
import Control.Newtype
import Data.Functor.Day
import qualified Graphics.Vty as V
import Graphics.Vty ((<|>))
import Pipes ((>->), Consumer, await, runEffect)
import Pipes.Concurrent (Input, fromInput)
import Relude hiding ((<|>))

newtype RenderM a = RenderM (ReaderT V.Vty IO a)
  deriving (Functor, Applicative, Monad, MonadReader V.Vty, MonadIO)

runRenderM :: Consumer (Maybe e) RenderM () -> V.Vty -> Input (Maybe e) -> IO ()
runRenderM crm vty input = runReaderT m vty
  where
    m :: ReaderT V.Vty IO ()
    (RenderM m) = runEffect $ fromInput input >-> crm

explore :: forall w m e. (Show e, Comonad w) => Pairing m w -> Component' w m e V.Image -> Consumer (Maybe e) RenderM ()
explore pair space = do
  let (img, runner) = render space
  vty <- lift ask
  liftIO $ V.update vty (V.picForImage img)
  e <- await
  case e of
    Nothing -> return ()
    Just e -> explore pair (appEndo (runner e) space)
  where
    render :: forall v. Component' w m e v -> (v, Handler e (Component' w m e v))
    render space = let { (UI ui) = extract (unpack space) } in runWriter (ui send)
    send :: forall v. m () -> Endo (Component' w m e v)
    send action =
      Endo $ over Component' $ pair (const id) action <<< duplicate

exploreCo :: forall w e. (Show e, Comonad w) => Component w e V.Image -> Consumer (Maybe e) RenderM ()
exploreCo = explore (pairSym pairCo)

newtype AddingInt = AddingInt Int

instance Semigroup AddingInt where
  (<>) (AddingInt i1) (AddingInt i2) = AddingInt $ i1 + i2

instance Monoid AddingInt where
  mempty = AddingInt 0

tracedExample :: Component (Traced AddingInt) e V.Image
tracedExample = Component' $ traced render
  where
    render :: AddingInt -> UI (Co (Traced AddingInt) ()) e V.Image
    render (AddingInt count) = UI $ \send -> do
      let line0 = V.text (V.defAttr `V.withForeColor` V.green) ("Traced " <> show count <> " line")
          line1 = V.string (V.defAttr `V.withBackColor` V.blue) "x"
          img = line0 <|> line1
      () <-
        tell $ const
          $ if count < 3
            then send (unpack $ tell' (AddingInt 1))
            else Endo id
      return img

storeExample :: Component (Store Int) e V.Image
storeExample = Component' $ store render 0
  where
    render :: Int -> UI (Co (Store Int) ()) e V.Image
    render count = UI $ \send -> do
      let line0 = V.text (V.defAttr `V.withForeColor` V.green) ("Store " <> show count <> " line")
          line1 = V.string (V.defAttr `V.withBackColor` V.blue) "x"
          img = line0 <|> line1
      () <-
        tell $ const
          $ if count < 3
            then send (modify' (+ 1))
            else Endo id
      return $ img & V.resizeWidth 20 & V.translateX 3
    modify' f = unpack @(Co' (Store Int) ()) $ modify f

combinedExample :: forall e. Component (Day (Store Int) (Traced AddingInt)) e V.Image
combinedExample = combine with storeExample tracedExample
  where
    with :: forall a. UI a e V.Image -> UI a e V.Image -> UI a e V.Image
    with (UI ui1) (UI ui2) = UI $ \send -> do
      pic1 <- ui1 send
      pic2 <- ui2 send
      --let w1 = imageWidth pic1
      --let w2 = imageWidth pic2
      return $ pic1 <|> pic2
