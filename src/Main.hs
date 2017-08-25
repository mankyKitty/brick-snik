{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (..), EventM, Next,
                                             Padding (..), Widget, (<+>))
import qualified Brick                      as B
import qualified Brick.BChan                as BChan

import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C

import qualified Graphics.Vty               as V

import           Snake                      (Game, initGame, step, turn)

-- | Ticks mark the passing of time
--
-- This is our custom event that constantly fed into the app
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to mangle later
-- if I give it a name
type Name = ()

data Cell
  = Snake
  | Food
  | Empty

-- Brick App definition
app
  :: App
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

main :: IO ()
main = do
  tickChan <- BChan.newBChan 10
  forkIO . forever $ do
    BChan.writeBChan chan Tick
    threadDelay 100000

  g <- initGame
  void $ B.customMain
    (V.mkVty V.defaultConfig)
    (Just chan)
    app
    g

--------

handleEvent
  :: Game
  -> BrickEvent Name Tick
  -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)              = B.continue (step g)
handleEvent g (VtyEvent (V.EvKey eKey [])) =
  case eKey of
    V.KChar 'r' -> liftIO ( initGame ) >>= B.continue
    V.KChar 'q' -> B.halt g

    V.KUp       -> t N
    V.KChar 'k' -> t N

    V.KDown     -> t S
    V.KChar 'j' -> t S

    V.KRight    -> t E
    V.KChar 'l' -> t E

    V.KLeft     -> t W
    V.KChar 'h' -> t W

    V.KEsc      -> B.halt g
    _           -> B.continue g
  where
    t d = B.continue $ turn d g

-- GUI WORK

drawUI
  :: Game
  -> [Widget Name]
drawUI =
  undefined

theMap
  :: AttrMap
theMap =
  undefined
