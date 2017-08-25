{-# LANGUAGE TemplateHaskell #-}
module Snake where

import Control.Lens (makeLenses, (^.), (.~))

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Tagged (Tagged (..))


data Game = Game
  { _snake :: Snake
  , _dir :: Direction
  , _food :: Coord
  , _foods :: Stream Coord
  , _dead :: Bool
  , _paused :: Bool
  , _score :: Int
  , _frozen :: Bool
  }
  deriving Show
makeLenses ''Game

data Coord'
type Coord = Tagged Coord' (V2 Int)

data Snake'
type Snake = Tagged Snake' Coord

data Stream a = a :| Stream a
  deriving Show

data Direction
  = N
  | S
  | E
  | W
  deriving (Eq, Show)

gameStep
  :: Game
  -> Game
gameStep g = fromMaybe g $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  return .fromMaybe (move g') $ die g' <|> eatFood g'

-- | Possibly die if next head position is disallowed
die
  :: Game
  -> Maybe Game
die = undefined

-- | Possibly eat food if next head position is food
eatFood 
  :: Game
  -> Maybe Game
eatFood = undefined

-- | Move snake along in a marquee fashion
move
  :: Game
  -> Game
move =
  undefined

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn
  :: Direction
  -> Game
  -> Game
turn =
  undefined

-- | Initialise a paused game with random food location
initGame
  :: IO Game
initGame =
  undefined
