{-# LANGUAGE FlexibleContexts #-}
module SnakeLib (
    MovementInput (..),
    Tile (..),
    Pos,
    Board,

    Game (..),
    initialState,

    runTick,

    SnakeSegmentOrientation (..),
    Direction (..),
    growHead,
    shrinkTail
) where

import Data.Array
import Control.Monad.State
import Control.Monad (when, unless)
import System.Random (randomRIO)

data Tile = SnakeSegment SnakeSegmentOrientation | Apple | Empty deriving (Eq, Show)

type Pos = (Int, Int)

type Board = Array Pos Tile

data Game = Game {gameBoard :: Board, gameBoardSize :: (Int, Int), snakeHead :: Pos, snakeTail :: Pos}

emptyBoard :: (Int, Int) -> Array (Int, Int) Tile
emptyBoard (w, h) = array ((0, 0), (w-1, h-1)) [((i, j), Empty) | i <- [0..w-1], j <- [0..h-1]]

initialState :: (Int, Int) -> (Int, Int) -> Game
initialState size (x, y) = Game 
    { gameBoard=(emptyBoard size) // [((x-1, y), SnakeSegment TAIL_RIGHT), ((x, y), SnakeSegment HEAD_RIGHT), ((x + 2, y), Apple), ((x + 3, y), Apple)]
    , gameBoardSize=size
    , snakeHead=(x, y)
    , snakeTail=(x-1, y)}

runTick :: Game -> MovementInput -> IO (MovementResult, Game)
runTick game input = runStateT (gameTick input) game

gameTick :: (MonadState Game m, MonadIO m) => MovementInput -> m MovementResult
gameTick input = do
    movementResult <- advanceSnake input
    when (movementResult == IntoEmpty) shrinkSnake
    when (movementResult == IntoApple) spawnApple
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
    let modifiedBoard = board // [(pos, SnakeSegment $ growHead orientation direction), (destination, SnakeSegment $ getNewHead direction)]
    modify (\s -> s {gameBoard=modifiedBoard, snakeHead=destination})

growHead :: SnakeSegmentOrientation -> Direction -> SnakeSegmentOrientation
growHead HEAD_UP UP = VERTICAL
growHead HEAD_UP LEFT = TURN_DOWN_LEFT
growHead HEAD_UP RIGHT = TURN_DOWN_RIGHT
growHead HEAD_RIGHT RIGHT = HORIZONTAL
growHead HEAD_RIGHT UP = TURN_UP_LEFT
growHead HEAD_RIGHT DOWN = TURN_DOWN_LEFT
growHead HEAD_DOWN DOWN = VERTICAL
growHead HEAD_DOWN RIGHT = TURN_UP_RIGHT
growHead HEAD_DOWN LEFT = TURN_UP_LEFT
growHead HEAD_LEFT LEFT = HORIZONTAL
growHead HEAD_LEFT DOWN = TURN_DOWN_RIGHT
growHead HEAD_LEFT UP = TURN_UP_RIGHT
growHead _ _ = error "Invalid growHead arguments"

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
    let modifiedBoard = board // [(pos, Empty), (destination, SnakeSegment $ shrinkTail orientation direction)]
    modify (\s -> s {gameBoard=modifiedBoard, snakeTail=destination})

shrinkTail :: SnakeSegmentOrientation -> Direction -> SnakeSegmentOrientation
shrinkTail HEAD_UP UP = TAIL_UP
shrinkTail HEAD_RIGHT RIGHT = TAIL_RIGHT
shrinkTail HEAD_DOWN DOWN = TAIL_DOWN
shrinkTail HEAD_LEFT LEFT = TAIL_LEFT
shrinkTail TURN_UP_RIGHT LEFT = TAIL_UP
shrinkTail TURN_UP_RIGHT DOWN = TAIL_RIGHT
shrinkTail TURN_DOWN_RIGHT LEFT = TAIL_DOWN
shrinkTail TURN_DOWN_RIGHT UP = TAIL_RIGHT
shrinkTail TURN_DOWN_LEFT RIGHT = TAIL_DOWN
shrinkTail TURN_DOWN_LEFT UP = TAIL_LEFT
shrinkTail TURN_UP_LEFT RIGHT = TAIL_UP
shrinkTail TURN_UP_LEFT DOWN = TAIL_LEFT
shrinkTail HORIZONTAL RIGHT = TAIL_RIGHT
shrinkTail HORIZONTAL LEFT = TAIL_LEFT
shrinkTail VERTICAL UP = TAIL_UP
shrinkTail VERTICAL DOWN = TAIL_DOWN
shrinkTail o d = error ("Invalid shrinkTail arguments: " ++ show o ++ " " ++ show d)

spawnApple :: (MonadState Game m, MonadIO m) => m ()
spawnApple = do
    (w, h) <- gets gameBoardSize
    pos <- liftIO $ randomRIO ((0, 0), (w - 1, h - 1))
    tile <- getTileAt pos
    case tile of 
        Empty -> modify (\s -> s {gameBoard = gameBoard s // [(pos, Apple)]})
        _ -> spawnApple

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
