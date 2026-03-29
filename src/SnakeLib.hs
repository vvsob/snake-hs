{-# LANGUAGE FlexibleContexts #-}
module SnakeLib (
    MovementInput (..),
    Tile (..),
    Pos,
    Board,

    Game (..),
    initialState,

    runTick,

    SnakeSegmentOrientation (..)
) where

import Data.Array
import Control.Monad.State
import Control.Monad (when)

data Tile = SnakeSegment SnakeSegmentOrientation | Apple | Empty

type Pos = (Int, Int)

type Board = Array Pos Tile

data Game = Game {gameBoard :: Board, gameBoardSize :: (Int, Int), snakeHead :: Pos, snakeTail :: Pos}

emptyBoard :: (Int, Int) -> Array (Int, Int) Tile
emptyBoard (w, h) = array ((0, 0), (w-1, h-1)) [((i, j), Empty) | i <- [0..w-1], j <- [0..h-1]]

initialState :: (Int, Int) -> (Int, Int) -> Game
initialState size (x, y) = Game 
    { gameBoard=(emptyBoard size) // [((x-1, y), SnakeSegment TAIL_RIGHT), ((x, y), SnakeSegment HEAD_RIGHT)]
    , gameBoardSize=size
    , snakeHead=(x, y)
    , snakeTail=(x-1, y)}

runTick :: Game -> MovementInput -> IO (MovementResult, Game)
runTick game input = runStateT (gameTick input) game

gameTick :: (MonadState Game m, MonadIO m) => MovementInput -> m MovementResult
gameTick input = do
    movementResult <- advanceSnake input
    when (movementResult == IntoEmpty) shrinkSnake
    pure movementResult

advanceSnake :: MonadState Game m => MovementInput -> m MovementResult
advanceSnake input = do
    idleDirection <- headDirection
    let inputDirection = case input of 
            UpPressed -> UP
            RightPressed -> RIGHT
            DownPressed -> DOWN
            LeftPressed -> LEFT
            NothingPressed -> idleDirection
    let direction = if areOpposite idleDirection inputDirection then idleDirection else inputDirection
    pos <- gets snakeHead
    destination <- shiftPos pos direction
    destinationTile <- gets ((`tileAt` destination) . gameBoard)
    case destinationTile of
        Apple -> moveHead direction >> pure IntoApple
        Empty -> moveHead direction >> pure IntoEmpty
        SnakeSegment _ -> pure IntoSnake

headDirection :: MonadState Game m => m Direction
headDirection = gets f
    where 
        f :: Game -> Direction
        f game = case tileAt (gameBoard game) (snakeHead game) of
            SnakeSegment HEAD_DOWN -> DOWN
            SnakeSegment HEAD_LEFT -> LEFT
            SnakeSegment HEAD_UP -> UP
            SnakeSegment HEAD_RIGHT -> RIGHT
            _ -> error "Invalid snake head tile"

moveHead :: MonadState Game m => Direction -> m ()
moveHead direction = do 
    pos <- gets snakeHead
    destination <- shiftPos pos direction
    board <- gets gameBoard
    headTile <- getTileAt pos
    let orientation = case headTile of 
            SnakeSegment x -> x
            _ -> error "Invalid snake head tile"
    let modifiedBoard = board // [(pos, SnakeSegment $ getGrownHead orientation direction), (destination, SnakeSegment $ getNewHead direction)]
    modify (\s -> s {gameBoard=modifiedBoard, snakeHead=destination})

getGrownHead :: SnakeSegmentOrientation -> Direction -> SnakeSegmentOrientation
getGrownHead HEAD_UP UP = VERTICAL
getGrownHead HEAD_UP LEFT = TURN_DOWN_LEFT
getGrownHead HEAD_UP RIGHT = TURN_DOWN_RIGHT
getGrownHead HEAD_RIGHT RIGHT = HORIZONTAL
getGrownHead HEAD_RIGHT UP = TURN_UP_LEFT
getGrownHead HEAD_RIGHT DOWN = TURN_DOWN_LEFT
getGrownHead HEAD_DOWN DOWN = VERTICAL
getGrownHead HEAD_DOWN RIGHT = TURN_UP_RIGHT
getGrownHead HEAD_DOWN LEFT = TURN_UP_LEFT
getGrownHead HEAD_LEFT LEFT = HORIZONTAL
getGrownHead HEAD_LEFT DOWN = TURN_DOWN_RIGHT
getGrownHead HEAD_LEFT UP = TURN_UP_RIGHT
getGrownHead _ _ = error "Invalid getGrownHead arguments"

getNewHead :: Direction -> SnakeSegmentOrientation
getNewHead UP = HEAD_UP
getNewHead RIGHT = HEAD_RIGHT
getNewHead DOWN = HEAD_DOWN
getNewHead LEFT = HEAD_LEFT

shrinkSnake :: MonadState Game m => m ()
shrinkSnake = do
    pos <- gets snakeTail
    tile <- getTileAt pos
    let direction = case tile of
            SnakeSegment TAIL_UP -> UP
            SnakeSegment TAIL_RIGHT -> RIGHT
            SnakeSegment TAIL_DOWN -> DOWN
            SnakeSegment TAIL_LEFT -> LEFT
            _ -> error "Invalid snake tail tile"
    destination <- shiftPos pos direction
    board <- gets gameBoard
    destinationTile <- getTileAt destination
    let orientation = case destinationTile of 
            SnakeSegment x -> x
            _ -> error "Invalid snake segment tile"
    let modifiedBoard = board // [(pos, Empty), (destination, SnakeSegment $ getNewTail orientation direction)]
    modify (\s -> s {gameBoard=modifiedBoard, snakeTail=destination})

getNewTail :: SnakeSegmentOrientation -> Direction -> SnakeSegmentOrientation
getNewTail HEAD_UP UP = TAIL_UP
getNewTail HEAD_RIGHT RIGHT = TAIL_RIGHT
getNewTail HEAD_DOWN DOWN = TAIL_DOWN
getNewTail HEAD_LEFT LEFT = TAIL_LEFT
getNewTail TURN_UP_RIGHT LEFT = TAIL_UP
getNewTail TURN_UP_RIGHT DOWN = TAIL_RIGHT
getNewTail TURN_DOWN_RIGHT LEFT = TAIL_DOWN
getNewTail TURN_DOWN_RIGHT UP = TAIL_RIGHT
getNewTail TURN_DOWN_LEFT RIGHT = TAIL_DOWN
getNewTail TURN_DOWN_LEFT UP = TAIL_LEFT
getNewTail TURN_UP_LEFT RIGHT = TAIL_UP
getNewTail TURN_UP_LEFT DOWN = TAIL_LEFT
getNewTail HORIZONTAL RIGHT = TAIL_RIGHT
getNewTail HORIZONTAL LEFT = TAIL_LEFT
getNewTail VERTICAL UP = TAIL_UP
getNewTail VERTICAL DOWN = TAIL_DOWN
getNewTail o d = error ("Invalid getNewTail arguments: " ++ show o ++ " " ++ show d)

tileAt :: Board -> Pos -> Tile
tileAt board (x, y) = (board ! (x, y))

wrapPos :: (Int, Int) -> Pos -> Pos
wrapPos (w, h) (x, y) =
  (x `mod` w, y `mod` h)

getTileAt :: MonadState Game m => Pos -> m Tile
getTileAt pos = gets ((`tileAt` pos) . gameBoard)

shiftPos :: MonadState Game m => Pos -> Direction -> m Pos
shiftPos (x, y) direction = do
    let (dx, dy) = getDelta direction
    size <- gets gameBoardSize
    pure $ wrapPos size (x + dx, y + dy)

data MovementInput = UpPressed | RightPressed | DownPressed | LeftPressed | NothingPressed deriving (Eq, Show)

instance Semigroup MovementInput where
    (<>) l NothingPressed = l
    NothingPressed <> r = r
    l <> _ = l

instance Monoid MovementInput where
    mempty = NothingPressed

data MovementResult = IntoApple | IntoSnake | IntoEmpty deriving (Eq, Show)

data SnakeSegmentOrientation =
    HEAD_DOWN | HEAD_LEFT | HEAD_UP | HEAD_RIGHT |
    TURN_UP_RIGHT | TURN_DOWN_RIGHT | TURN_DOWN_LEFT | TURN_UP_LEFT |
    VERTICAL | HORIZONTAL |
    TAIL_UP | TAIL_RIGHT | TAIL_DOWN | TAIL_LEFT
    deriving (Eq, Show)

data Direction = DOWN | LEFT | UP | RIGHT deriving (Eq, Show)

areOpposite :: Direction -> Direction -> Bool
areOpposite UP DOWN = True
areOpposite DOWN UP = True
areOpposite LEFT RIGHT = True
areOpposite RIGHT LEFT = True
areOpposite _ _ = False

getDelta :: Direction -> (Int, Int)
getDelta UP = (0, -1)
getDelta RIGHT = (1, 0)
getDelta DOWN = (0, 1)
getDelta LEFT = (-1, 0)
