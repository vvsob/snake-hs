module Assets where

import SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as FONT

type Assets = (Texture, FONT.Font)

loadAssets :: Renderer -> IO Assets
loadAssets renderer = do
    texture <- IMG.loadTexture renderer "assets/spritesheet.png"
    font <- FONT.load "assets/font.ttf" 24
    pure (texture, font)

freeAssets :: Assets -> IO ()
freeAssets (texture, font) = do
    destroyTexture texture
    FONT.free font
