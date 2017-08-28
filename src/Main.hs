{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent          (forkIO, threadDelay)

import qualified Control.Concurrent.STM      as TVar
import           Control.Concurrent.STM.TVar (TVar)

import           Control.Lens                (to, (.~), (^.), _Wrapped)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class      (liftIO)

import           Brick                       (App (..), AttrMap, AttrName,
                                              BrickEvent (..), EventM, Next,
                                              Padding (..), Widget, (<+>))
import qualified Brick                       as B
import qualified Brick.BChan                 as BChan

import qualified Brick.Widgets.Border        as B
import qualified Brick.Widgets.Border.Style  as BS
import qualified Brick.Widgets.Center        as C

import qualified Graphics.Vty                as V

import           Linear.V2                   (V2 (V2))

import           Data.Function               ((&))
import           Snake                       (Direction (..), Game, dead, food,
                                              height, initGame, initialSpeed,
                                              interval, mkCoord, score, snake,
                                              speed, step, turn, width)

-- | Ticks mark the passing of tunfoldime
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
  :: TVar Float
  -> App Game Tick Name
app tv = App
  { appDraw         = drawUI
  , appChooseCursor = B.neverShowCursor
  , appHandleEvent  = handleEvent tv
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

main :: IO ()
main = do
  tickChan <- BChan.newBChan 10
  tv <- TVar.atomically $ TVar.newTVar initialSpeed

  _ <- forkIO . forever $ do
    BChan.writeBChan tickChan Tick
    sp <- TVar.atomically $ TVar.readTVar tv
    threadDelay (speedToInt sp)

  g <- initGame tv

  B.customMain (V.mkVty V.defaultConfig) (Just tickChan) ( app tv ) g
    >>= printResult
  where
    speedToInt
      :: Float
      -> Int
    speedToInt =
      (* 100000) . fst . properFraction

--------
printResult
  :: Game
  -> IO ()
printResult g = putStrLn $
  if g ^. score . to (> 0)
  then "OMG"
  else "OH NOES"

handleEvent
  :: TVar Float
  -> Game
  -> BrickEvent Name Tick
  -> EventM Name (Next Game)
handleEvent _    g (AppEvent Tick)                         = B.continue (step g)
handleEvent _    g (VtyEvent (V.EvKey V.KRight [V.MCtrl])) = handleSpeed g (+)
handleEvent _    g (VtyEvent (V.EvKey V.KLeft [V.MCtrl]))  = handleSpeed g (-)
handleEvent tVar g (VtyEvent (V.EvKey eKey []))            =
  case eKey of
    V.KChar 'r' -> liftIO ( initGame tVar ) >>= B.continue
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
handleEvent _ g _ = B.continue g

handleSpeed
  :: Game
  -> (Float -> Float -> Float)
  -> EventM n (Next Game)
handleSpeed g (+/-) = do
  let newSp = validS $ (g ^. speed) +/- speedInc
  liftIO $ TVar.atomically (TVar.writeTVar (g ^. interval) newSp)
  B.continue $ g & speed .~ newSp
  where
    validS s
      | s >= 0.01 && s <= 1.0 = s
      | otherwise             = g ^. speed

speedInc
  :: Float
speedInc =
  0.01

-- GUI WORK

drawUI
  :: Game
  -> [Widget Name]
drawUI g =
  [ C.center $ B.padRight (Pad 2) (drawStats g) <+> (drawGrid g) ]

drawStats
  :: Game
  -> Widget Name
drawStats g = B.hLimit 11
  $ B.vBox [ drawScore (g ^. score)
           , B.padTop (Pad 2) $ drawGameOver (g ^. dead)
           ]
  where
    drawScore n = B.withBorderStyle BS.unicodeBold
      . B.borderWithLabel (B.str "Score")
      . C.hCenter
      . B.padAll 1
      $ B.str (show n)

    drawGameOver True  = B.withAttr gameOverAttr $ C.hCenter (B.str "Game Over")
    drawGameOver False = B.emptyWidget


drawGrid
  :: Game
  -> Widget Name
drawGrid g = B.withBorderStyle BS.unicodeBold
  . B.borderWithLabel (B.str "Snake")
  $ B.vBox rows
  where
    rows = [B.hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]

    cellsInRow y = [ drawCoord (mkCoord $ V2 x y) | x <- [0 .. width - 1]]

    drawCoord = drawCell . cellAt

    cellAt c
      | g ^. snake . _Wrapped . to (any (== c) ) = Snake
      | c == g ^. food                           = Food
      | otherwise                                = Empty

drawCell
  :: Cell
  -> Widget Name
drawCell Snake = B.withAttr snakeAttr cw
drawCell Food  = B.withAttr foodAttr cw
drawCell Empty = B.withAttr emptyAttr cw

cw :: Widget Name
cw = B.str " "

snakeAttr, foodAttr, emptyAttr, gameOverAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr  = "foodAttr"
emptyAttr = "emptyAttr"
gameOverAttr = "gameOver"

theMap
  :: AttrMap
theMap = B.attrMap V.defAttr
  [ (snakeAttr, V.blue `B.on` V.blue)
  , (foodAttr, V.red `B.on` V.red)
  , (gameOverAttr, B.fg V.red `V.withStyle` V.bold)
  ]
