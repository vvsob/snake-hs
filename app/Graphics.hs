module Graphics (
    renderFrame
) where

import SDL
import SnakeLib
import Foreign.C (CInt)
import Data.Array (indices)
import Control.Monad (void)

renderFrame :: Renderer -> Texture -> IO ()
renderFrame renderer texture = do
    rendererDrawColor renderer $= V4 32 32 32 255
    clear renderer
    rendererDrawColor renderer $= V4 255 255 255 255
    renderState renderer texture $ initialState (20, 20) (2, 2)
    present renderer

renderState :: Renderer -> Texture -> GameState -> IO ()
renderState renderer texture state = do
    void $ traverse (renderTile renderer texture state) (indices $ gameBoard state)

renderTile :: Renderer -> Texture -> GameState -> Pos -> IO ()
renderTile renderer texture state pos = case tileAt (gameBoard state) pos of
    Empty -> return ()
    Apple -> renderSpriteAt renderer texture APPLE pos
    Snake -> 
        let size = gameBoardSize state
        in renderSpriteAt renderer texture (snakeSprite (neighborTiles size (gameBoard state) pos) (pos == snakeHead state))pos

snakeSprite :: (Tile, Tile, Tile, Tile) -> Bool -> Sprite
snakeSprite (Snake, _, _, _) True = SNAKE_HEAD_DOWN
snakeSprite (_, Snake, _, _) True = SNAKE_HEAD_LEFT
snakeSprite (_, _, Snake, _) True = SNAKE_HEAD_UP
snakeSprite (_, _, _, Snake) True = SNAKE_HEAD_RIGHT
snakeSprite (Snake, _, Snake, _) _ = SNAKE_VERTICAL
snakeSprite (_, Snake, _, Snake) _ = SNAKE_HORIZONTAL
snakeSprite (Snake, Snake, _, _) _ = SNAKE_TURN_UP_RIGHT
snakeSprite (_, Snake, Snake, _) _ = SNAKE_TURN_DOWN_RIGHT
snakeSprite (_, _, Snake, Snake) _ = SNAKE_TURN_DOWN_LEFT
snakeSprite (Snake, _, _, Snake) _ = SNAKE_TURN_UP_LEFT
snakeSprite (Snake, _, _, _) _ = SNAKE_TAIL_UP
snakeSprite (_, Snake, _, _) _ = SNAKE_TAIL_RIGHT
snakeSprite (_, _, Snake, _) _ = SNAKE_TAIL_DOWN
snakeSprite (_, _, _, Snake) _ = SNAKE_TAIL_LEFT
snakeSprite _ _ = APPLE

renderSpriteAt :: Renderer -> Texture -> Sprite -> (Int, Int) -> IO ()
renderSpriteAt renderer texture sprite (x, y) = do
    let srcRect = getSpriteSheetLocation $ getSpriteIndex sprite
    let dstRect = Rectangle (P (V2 (64 * (fromIntegral x)) (64 * (fromIntegral y)))) (V2 64 64)
    copy renderer texture (Just srcRect) (Just dstRect)

getSpriteSheetLocation :: (Int, Int) -> Rectangle CInt
getSpriteSheetLocation (y, x) = Rectangle (P (V2 (16 * (fromIntegral x)) (16 * (fromIntegral y)))) (V2 16 16)

getSpriteIndex :: Sprite -> (Int, Int)
getSpriteIndex SNAKE_HEAD_DOWN = (0, 0)
getSpriteIndex SNAKE_HEAD_LEFT = (0, 1)
getSpriteIndex SNAKE_HEAD_UP = (0, 2)
getSpriteIndex SNAKE_HEAD_RIGHT = (0, 3)
getSpriteIndex SNAKE_TURN_UP_RIGHT = (1, 0)
getSpriteIndex SNAKE_TURN_DOWN_RIGHT = (1, 1)
getSpriteIndex SNAKE_TURN_DOWN_LEFT = (1, 2)
getSpriteIndex SNAKE_TURN_UP_LEFT = (1, 3)
getSpriteIndex SNAKE_VERTICAL = (2, 0)
getSpriteIndex SNAKE_HORIZONTAL = (2, 1)
getSpriteIndex SNAKE_TAIL_UP = (3, 0)
getSpriteIndex SNAKE_TAIL_RIGHT = (3, 1)
getSpriteIndex SNAKE_TAIL_DOWN = (3, 2)
getSpriteIndex SNAKE_TAIL_LEFT = (3, 3)
getSpriteIndex APPLE = (2, 2)

data Sprite = 
    SNAKE_HEAD_DOWN | SNAKE_HEAD_LEFT | SNAKE_HEAD_UP | SNAKE_HEAD_RIGHT |
    SNAKE_TURN_UP_RIGHT | SNAKE_TURN_DOWN_RIGHT | SNAKE_TURN_DOWN_LEFT | SNAKE_TURN_UP_LEFT |
    SNAKE_VERTICAL | SNAKE_HORIZONTAL |
    SNAKE_TAIL_UP | SNAKE_TAIL_RIGHT | SNAKE_TAIL_DOWN | SNAKE_TAIL_LEFT |
    APPLE

