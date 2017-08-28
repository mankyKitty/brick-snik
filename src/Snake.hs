{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Snake where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Lens                (Lens', makeLenses, to, (%~), (.~),
                                              (^.), _Wrapped)

import           Data.Function               ((&))
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NE
import           Data.Tagged                 (Tagged (..))

import           Linear.V2                   (V2 (..))
import qualified Linear.V2                   as L

import           System.Random               (Random (..), StdGen, newStdGen)

data Coord'
type Coord = Tagged Coord' (V2 Int)

mkCoord
  :: V2 Int
  -> Coord
mkCoord = Tagged

data Snake'
type Snake = Tagged Snake' (NonEmpty Coord)

mkSnake
  :: Coord
  -> Snake
mkSnake = Tagged . ( NE.:| [] )

data Direction
  = N
  | S
  | E
  | W
  deriving (Eq, Show)

data Game = Game
  { _snake    :: Snake
  , _dir      :: Direction
  , _food     :: Coord
  , _dead     :: Bool
  , _paused   :: Bool
  , _frozen   :: Bool
  , _score    :: Int
  , _interval :: TVar Float
  , _speed    :: Float
  , _rGen     :: StdGen
  }
makeLenses ''Game


snakeL
  :: Lens' Game (NonEmpty Coord)
snakeL =
  snake . _Wrapped

nextH
  :: Game
  -> Coord
nextH g =
  nextHead (g ^. dir) (g ^. snake)

height, width :: Int
height = 20
width = 20

step
  :: Game
  -> Game
step g =
  let ohMy = die g
  in
    if ohMy ^. paused || ohMy ^. dead
    then ohMy
    else move $ eatFood ohMy

-- | Possibly die if next head position is disallowed
die
  :: Game
  -> Game
die g =
  let nHead  = nextH g

      hitSnake h = g ^. snakeL . to (any (== h ))

      hitWall
        :: Coord
        -> Bool
      hitWall h  =
        let x' = h ^. _Wrapped . L._x
            y' = h ^. _Wrapped . L._y
        in
          ( x' == 0 || x' == width ) || ( y' == 0 || y' == height )
  in
    if hitWall nHead || hitSnake nHead
    then g & dead .~ True
    else g

-- | Possibly eat food if next head position is food
eatFood
  :: Game
  -> Game
eatFood g =
  let nh = nextH g
  in
    if g ^. food . to ( == nh )
    then nextFood $ g
         & snakeL %~ NE.cons nh
         & score %~ (+ 1)
    else g

-- | Set a valid next food Coord
nextFood
  :: Game
  -> Game
nextFood g =
  let (nFood, foodGen) = g ^. rGen . to rCoord
  in
    g & food .~ nFood
      & rGen .~ foodGen

-- | Move snake along in a marquee fashion
move
  :: Game
  -> Game
move g =
  g & snakeL %~ (( ( nextH g ) NE.:| ) . NE.init)

nextHead
  :: Direction
  -> Snake
  -> Coord
nextHead d s =
  case d of
    N -> sL & _Wrapped . L._y %~ ( f (+1) height )
    S -> sL & _Wrapped . L._y %~ ( f (subtract 1) height )
    E -> sL & _Wrapped . L._x %~ ( f (+1) width )
    W -> sL & _Wrapped . L._x %~ ( f (subtract 1) width )
  where
    f g b c = (g c) `mod` b
    sL = s ^. _Wrapped . to NE.head

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn
  :: Direction
  -> Game
  -> Game
turn nDir g
  | g ^. frozen = g
  | g ^. paused = g
  | otherwise   = g & dir .~ nDir

initialSpeed
  :: Float
initialSpeed =
  1.00

rCoord
  :: StdGen
  -> (Coord, StdGen)
rCoord sg =
  let (w, sg') = rInBnds width sg
      (h, sg'') = rInBnds height sg'
  in
    (mkCoord (V2 w h), sg'')
  where
    rInBnds b g =
      randomR ( 0, (b - 1) ) g

-- | Initialise a paused game with random food location
initGame
  :: TVar Float
  -> IO Game
initGame tickVar = do
  (startFood, foodGen) <- rCoord <$> newStdGen
  pure . nextFood $ Game
    ( mkSnake . mkCoord $ V2 (width `div` 2) (height `div` 2) )
    N
    startFood
    False False False
    0
    tickVar
    initialSpeed
    foodGen

