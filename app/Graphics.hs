module Graphics (
    renderFrame,
    tileSize
) where

import SDL
import qualified SDL.Font as FONT
import Foreign.C (CInt)
import Data.Array (assocs)
import Control.Monad (void)

import Snake

import Assets
import qualified Data.Text as Text

spriteSize :: CInt
spriteSize = 16

tileSize :: CInt
tileSize = 64

renderFrame :: Renderer -> Assets -> Game -> IO ()
renderFrame renderer (texture, font) game = do
    rendererDrawColor renderer $= V4 32 32 32 255
    clear renderer
    rendererDrawColor renderer $= V4 255 255 255 255
    renderGame renderer texture game
    renderScore renderer font game
    present renderer

renderGame :: Renderer -> Texture -> Game -> IO ()
renderGame renderer texture state = do
    void $ traverse (renderTile renderer texture) (assocs $ gameBoard state)

renderTile :: Renderer -> Texture -> (Pos, Tile) -> IO ()
renderTile renderer texture (pos, tile) = case tile of
    Empty -> return ()
    Apple -> renderSpriteAt renderer texture APPLE pos
    SnakeSegment orientation -> renderSpriteAt renderer texture (snakeSprite orientation) pos

renderScore :: Renderer -> FONT.Font -> Game -> IO ()
renderScore renderer font game = do
    let score = snakeSegmentCount game
    surface <- FONT.blended font (V4 255 255 255 255) $ Text.pack ("Score: " ++ show score)
    texture <- createTextureFromSurface renderer surface
    freeSurface surface

    width <- textureWidth <$> queryTexture texture
    height <- textureHeight <$> queryTexture texture
    copy renderer texture Nothing (Just (Rectangle (P (V2 20 20)) (V2 width height)))

    destroyTexture texture

snakeSprite :: SnakeSegmentOrientation -> Sprite
snakeSprite HEAD_DOWN = SNAKE_HEAD_DOWN
snakeSprite HEAD_LEFT = SNAKE_HEAD_LEFT
snakeSprite HEAD_UP = SNAKE_HEAD_UP
snakeSprite HEAD_RIGHT = SNAKE_HEAD_RIGHT
snakeSprite VERTICAL = SNAKE_VERTICAL
snakeSprite HORIZONTAL = SNAKE_HORIZONTAL
snakeSprite TURN_UP_RIGHT = SNAKE_TURN_UP_RIGHT
snakeSprite TURN_DOWN_RIGHT = SNAKE_TURN_DOWN_RIGHT
snakeSprite TURN_DOWN_LEFT = SNAKE_TURN_DOWN_LEFT
snakeSprite TURN_UP_LEFT = SNAKE_TURN_UP_LEFT
snakeSprite TAIL_UP = SNAKE_TAIL_UP
snakeSprite TAIL_RIGHT = SNAKE_TAIL_RIGHT
snakeSprite TAIL_DOWN = SNAKE_TAIL_DOWN
snakeSprite TAIL_LEFT = SNAKE_TAIL_LEFT

renderSpriteAt :: Renderer -> Texture -> Sprite -> (Int, Int) -> IO ()
renderSpriteAt renderer texture sprite (x, y) = do
    let srcRect = getSpriteSheetLocation $ getSpriteIndex sprite
    let dstRect = Rectangle (P (V2 (tileSize * (fromIntegral x)) (tileSize * (fromIntegral y)))) (V2 tileSize tileSize)
    copy renderer texture (Just srcRect) (Just dstRect)

getSpriteSheetLocation :: (Int, Int) -> Rectangle CInt
getSpriteSheetLocation (y, x) = Rectangle (P (V2 (spriteSize * (fromIntegral x)) (spriteSize * (fromIntegral y)))) (V2 spriteSize spriteSize)

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

