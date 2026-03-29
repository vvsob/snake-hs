module SnakeLib (
    MovementInput (..),
    Tile (..),
    Pos,
    Board,
    tileAt,
    wrapPos,

    Game (..),
    initialState,

    execTick,

    SnakeOrientation (..)
) where

import Data.Array
import Control.Monad.State
import Control.Monad (when)

data MovementInput = UpPressed | RightPressed | DownPressed | LeftPressed | NothingPressed deriving (Eq, Show)

instance Semigroup MovementInput where
    (<>) l NothingPressed = l
    NothingPressed <> r = r
    l <> _ = l

instance Monoid MovementInput where
    mempty = NothingPressed

data Tile = Snake SnakeOrientation | Apple | Empty

type Pos = (Int, Int)
type Board = Array Pos Tile

tileAt :: Board -> Pos -> Tile
tileAt board (x, y) = (board ! (x, y))

wrapPos :: (Int, Int) -> Pos -> Pos
wrapPos (w, h) (x, y) =
  (x `mod` w, y `mod` h)

data Game = Game {gameBoard :: Board, gameBoardSize :: (Int, Int), snakeHead :: Pos, snakeTail :: Pos}

type GameState = State Game

emptyBoard :: (Int, Int) -> Array (Int, Int) Tile
emptyBoard (w, h) = array ((0, 0), (w-1, h-1)) [((i, j), Empty) | i <- [0..w-1], j <- [0..h-1]]

initialState :: (Int, Int) -> (Int, Int) -> Game
initialState size (x, y) = Game 
    { gameBoard=(emptyBoard size) // [((x-1, y), Snake TAIL_RIGHT), ((x, y), Snake HEAD_RIGHT)]
    , gameBoardSize=size
    , snakeHead=(x, y)
    , snakeTail=(x-1, y)}

execTick :: Game -> MovementInput -> Game
execTick game input = execState (gameTick input) game

gameTick :: MovementInput -> GameState MovementResult
gameTick input = do
    movementResult <- advanceSnake input
    when (movementResult == IntoEmpty) shrinkSnake
    pure movementResult

advanceSnake :: MovementInput -> GameState MovementResult
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
        Snake _ -> pure IntoSnake

headDirection :: GameState Direction
headDirection = gets f
    where 
        f :: Game -> Direction
        f game = case tileAt (gameBoard game) (snakeHead game) of
            Snake HEAD_DOWN -> DOWN
            Snake HEAD_LEFT -> LEFT
            Snake HEAD_UP -> UP
            Snake HEAD_RIGHT -> RIGHT
            _ -> error "Invalid snake head tile"

moveHead :: Direction -> GameState ()
moveHead direction = do 
    pos <- gets snakeHead
    destination <- shiftPos pos direction
    board <- gets gameBoard
    headTile <- gets ((`tileAt` pos) . gameBoard)
    let orientation = case headTile of 
            Snake x -> x
            _ -> error "Invalid snake head tile"
    let modifiedBoard = board // [(pos, Snake $ getGrownHead orientation direction), (destination, Snake $ getNewHead direction)]
    modify (\s -> s {gameBoard=modifiedBoard, snakeHead=destination})

getGrownHead :: SnakeOrientation -> Direction -> SnakeOrientation
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

getNewHead :: Direction -> SnakeOrientation
getNewHead UP = HEAD_UP
getNewHead RIGHT = HEAD_RIGHT
getNewHead DOWN = HEAD_DOWN
getNewHead LEFT = HEAD_LEFT

shiftPos :: Pos -> Direction -> GameState Pos
shiftPos (x, y) direction = do
    let (dx, dy) = getDelta direction
    size <- gets gameBoardSize
    pure $ wrapPos size (x + dx, y + dy)
    

shrinkSnake :: GameState ()
shrinkSnake = do
    pos <- gets snakeTail
    tile <- gets ((`tileAt` pos) . gameBoard)
    let direction = case tile of
            Snake TAIL_UP -> UP
            Snake TAIL_RIGHT -> RIGHT
            Snake TAIL_DOWN -> DOWN
            Snake TAIL_LEFT -> LEFT
            _ -> error "Invalid snake tail tile"
    destination <- shiftPos pos direction
    board <- gets gameBoard
    destinationTile <- gets ((`tileAt` destination) . gameBoard)
    let orientation = case destinationTile of 
            Snake x -> x
            _ -> error "Invalid snake segment tile"
    let modifiedBoard = board // [(pos, Empty), (destination, Snake $ getNewTail orientation direction)]
    modify (\s -> s {gameBoard=modifiedBoard, snakeTail=destination})

getNewTail :: SnakeOrientation -> Direction -> SnakeOrientation
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

data MovementResult = IntoApple | IntoSnake | IntoEmpty deriving (Eq, Show)

data SnakeOrientation =
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
