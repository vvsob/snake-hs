module Tile (
    Tile (..),
    SnakeSegmentOrientation (..),
    Direction (..),
    areOpposite,
    growHead,
    getNewHead,
    shrinkTail,
    getDelta
) where

data Tile = SnakeSegment SnakeSegmentOrientation | Apple | Empty deriving (Eq, Show)

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

getDelta :: Direction -> (Int, Int)
getDelta UP = (0, -1)
getDelta RIGHT = (1, 0)
getDelta DOWN = (0, 1)
getDelta LEFT = (-1, 0)
