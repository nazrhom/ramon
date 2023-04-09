{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( executeStep
    , World (..)
    , Instruction (..)
    , Orientation (..)
    , Room (..)
    , Position (..)
    , Tile (..)
    , defaultWorld
    , drawWorld
    , turn
    , move
    , clean
    , (>>)
    , Code (..)
    , Layout
    , toLayout
    , moveN
    ) where

import Prelude hiding ((>>))
import Graphics.Gloss
    ( Picture,
      black,
      blue,
      green,
      yellow,
      red,
      circleSolid,
      color,
      line,
      pictures,
      translate, white, rectangleSolid )
import Data.Map qualified as M
import Data.Stream.Infinite qualified as S
import Data.List.NonEmpty (fromList)
import Control.Lens ( (&), (^.), (%~), (.~), makeLenses )

-- Infrared

-- d <- obstacle ahead
-- move d
-- spin 90 r

newtype Position = Position { unPos :: (Int,Int) } deriving Show

instance Semigroup Position where
  (<>) :: Position -> Position -> Position
  (Position (a,b)) <> (Position (c,d)) = Position (a+c, b+d)

data Tile = Obstacle | Floor deriving Eq

instance Show Tile where
  show Obstacle = "*"
  show Floor = "."

type Layout = M.Map (Int,Int) Tile
data Room = Room { _lengthX :: Int, _lengthY :: Int, _layout :: Layout }
  deriving Show
makeLenses ''Room

toLayout :: [String] -> Layout
toLayout [] = M.empty
toLayout s = go (indexed s)
  where
    go :: [(Int, String)] -> Layout
    go ((i,r):rest) = foldl (\acc (j,t) -> M.insert (j,i) (toTile t) acc) (go rest) (indexed r)
    go [] = M.empty
    toTile '*' = Obstacle
    toTile '.' = Floor
    toTile _   = error "Unknown tile"


indexed :: [a] -> [(Int, a)]
indexed = go 0
  where
    go :: Int -> [a] -> [(Int, a)]
    go i (x:xs) = (i,x):go (i+1) xs
    go _ [] = []

data Instruction = Move | Turn Orientation | Clean deriving Show

data Orientation = N | S | E | W | NE | NW | SE | SW deriving Show

newtype Code = Code { uncode :: [Instruction] } deriving Show

-- smart constructors for edsl
turn :: Orientation -> Code
turn d = Code [Turn d]

move :: Code
move = Code [Move]

moveN :: Int -> Code
moveN n = Code $ replicate n Move

clean :: Code
clean = Code [Clean]

(>>) :: Code -> Code -> Code
(Code left) >> (Code right) = Code $ left <> right

toPosition :: Orientation -> Position
toPosition N = Position ( 0,-1)
toPosition S = Position ( 0, 1)
toPosition E = Position ( 1, 0)
toPosition W = Position (-1, 0)
toPosition NE = toPosition N <> toPosition E
toPosition NW = toPosition N <> toPosition W
toPosition SE = toPosition S <> toPosition E
toPosition SW = toPosition S <> toPosition W

-- timesPosition :: Int -> Position -> Position
-- timesPosition i (Position (a,b)) = Position (i*a, i*b)

data World
  = World {
    _room :: Room
  , _position :: Position
  , _orientation :: Orientation
  , _plan :: S.Stream Instruction
  }
  deriving Show
makeLenses ''World

tileSize :: Int
tileSize = 24

executeStep :: World -> World
executeStep w = case S.head $ w ^. plan of
  Move ->
    let nextPosition = w' ^. position <> toPosition (w' ^. orientation) in
      if inBounds nextPosition
      then
        w' & position .~ nextPosition
      else
        w'

  Turn o -> w' & orientation .~ o

  Clean -> w'
  where
    w' = w & plan %~ S.tail
    inBounds (Position (a,b)) = a >= 0 && b >= 0 && a <= w ^.room.lengthX && b <= w ^.room.lengthY 
      && case M.lookup (a,b) (w^.room.layout) of
        Just s -> s /= Obstacle
        Nothing -> False -- error $ show w


defaultWorld :: Layout -> Code -> World
defaultWorld l (Code s) = World (Room lX lY l) (Position (lX `div` 2, lY `div` 2)) S (toStream s)
  where
    -- TODO: calculate these from layout
    lX = 24
    lY = 24

    toStream = S.cycle . fromList

translateCoordinates :: World -> Position -> (Float, Float)
translateCoordinates w (Position (x,y)) = (fromIntegral $ tileSize * (x - halfX) , fromIntegral $ tileSize * (halfY - y))
  where
    halfX = w ^.room.lengthX `div` 2
    halfY = w ^.room.lengthY  `div` 2

withOffset :: (Float, Float) -> (Float, Float)
withOffset (a,b) = (a + halfTile, b - halfTile)
  where
    halfTile = fromIntegral $ tileSize `div` 2

drawWorld :: World -> Picture
drawWorld w = pictures [
      tilesFill
    , tileLines
    -- , translate 0 0 $ color green $ circleSolid 8
    -- , translate 120 120 $ color black $ circleSolid 8
    -- , translate 120 (-120) $ color blue $ circleSolid 8
    -- , translate (-120) 120 $ color red $ circleSolid 8
    -- , translate (-120) (-120) $ color yellow $ circleSolid 8
    , uncurry translate (withOffset $ translateCoordinates w (w ^.position)) $ color black $ circleSolid (fromIntegral $ tileSize `div` 2)
  ]
  where
    tileLines = pictures [verticals, horizontals]
    verticals = pictures $ line <$>
      [ translateCoordinates w <$> [Position (a, 0), Position (a, w ^.room.lengthX)] | k <- [0..numberOfVerts], let a = k ]
    horizontals = pictures $ line <$>
      [ translateCoordinates w <$> [Position (0, a), Position (w ^.room.lengthY, a)] | k <- [0..numberOfHors], let a = k ]
    numberOfVerts = w ^. room.lengthX
    numberOfHors = w ^. room.lengthY
    placeTile ((x,y), tile) = uncurry translate (withOffset $ translateCoordinates w (Position (x ,y))) (fromTile tile)
    tilesFill = pictures $ placeTile <$> M.toList (w ^.room.layout)

fromTile :: Tile -> Picture
fromTile = \case
  Floor -> color white baseShape
  Obstacle -> color black baseShape
  where
    baseShape = rectangleSolid (fromIntegral tileSize) (fromIntegral tileSize)
