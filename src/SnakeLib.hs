module SnakeLib (
    MovementInput (..),
    Tile (..),
    Pos,
    Board,
    tileAt,
    wrapPos,
    neighborTiles,

    GameState (..),
    initialState
) where

import Data.Array

data MovementInput = UpPressed | RightPressed | DownPressed | LeftPressed | NothingPressed

data Tile = Snake | Apple | Empty

type Pos = (Int, Int)
type Board = Array Pos Tile

tileAt :: Board -> Pos -> Tile
tileAt board (x, y) = (board ! (x, y))

wrapPos :: (Int, Int) -> Pos -> Pos
wrapPos (w, h) (x, y) =
  (x `mod` w, y `mod` h)

neighborTiles :: (Int, Int) -> Board -> Pos -> (Tile, Tile, Tile, Tile)
neighborTiles size board (x, y) =
  ( tileAt board (wrapPos size (x, y - 1))  -- up
  , tileAt board (wrapPos size (x + 1, y))  -- right
  , tileAt board (wrapPos size (x, y + 1))  -- down
  , tileAt board (wrapPos size (x - 1, y))  -- left
  )

data GameState = GameState {gameBoard :: Board, gameBoardSize :: (Int, Int), snakeHead :: Pos}

emptyBoard :: (Int, Int) -> Array (Int, Int) Tile
emptyBoard (w, h) = array ((0, 0), (w-1, h-1)) [((i, j), Empty) | i <- [0..w-1], j <- [0..h-1]]

initialState :: (Int, Int) -> (Int, Int) -> GameState
initialState size (x, y) = GameState {gameBoard=(emptyBoard size) // [((x-1, y), Snake), ((x, y), Snake)], gameBoardSize=size, snakeHead=(x, y)}

gameTick :: GameState -> MovementInput -> GameState
gameTick = undefined
