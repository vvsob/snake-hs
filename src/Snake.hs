{-# LANGUAGE FlexibleContexts #-}
module Snake (
    Tile (..),
    Pos,
    Board,

    Game (..),
    initialState,

    runTick,

    MovementInput (..),
    MovementResult (..),

    SnakeSegmentOrientation (..),
    Direction (..),
    growHead,
    shrinkTail,

    snakeSegmentCount
) where

import Data.Array
import Control.Monad.State
import Control.Monad (when)
import System.Random (randomRIO)
import Tile

type Pos = (Int, Int)

type Board = Array Pos Tile

data Game = Game {gameBoard :: Board, gameBoardSize :: (Int, Int), snakeHead :: Pos, snakeTail :: Pos}

data MovementInput = UpPressed | RightPressed | DownPressed | LeftPressed | NothingPressed deriving (Eq, Show)

instance Semigroup MovementInput where
    (<>) l NothingPressed = l
    NothingPressed <> r = r
    l <> _ = l

instance Monoid MovementInput where
    mempty = NothingPressed

data MovementResult = IntoApple | IntoSnake | IntoEmpty deriving (Eq, Show)

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
    idleDirection <- getHeadDirection
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

getHeadDirection :: MonadState Game m => m Direction
getHeadDirection = gets f
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

spawnApple :: (MonadState Game m, MonadIO m) => m ()
spawnApple = do
    (w, h) <- gets gameBoardSize
    pos <- liftIO $ randomRIO ((0, 0), (w - 1, h - 1))
    tile <- getTileAt pos
    case tile of 
        Empty -> modify (\s -> s {gameBoard = gameBoard s // [(pos, Apple)]})
        _ -> spawnApple

snakeSegmentCount :: Game -> Int
snakeSegmentCount = length . filter isSnakeSegment . elems . gameBoard

isSnakeSegment :: Tile -> Bool
isSnakeSegment (SnakeSegment _) = True
isSnakeSegment _ = False
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



