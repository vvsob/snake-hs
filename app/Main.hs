module Main where

import SDL
import qualified SDL.Image as IMG
import qualified Data.Text as Text
import Control.Monad (unless)
import Graphics
import SnakeLib
import SDL.Raw (getTicks)
import Data.Word (Word32)

main :: IO ()
main = do
    initializeAll
    window <- createWindow (Text.pack "Hello, World") defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    
    texture <- IMG.loadTexture renderer "assets/spritesheet.png"

    appLoop renderer texture

    destroyTexture texture
    destroyRenderer renderer
    destroyWindow window

targetFps :: Word32
targetFps = 60

targetFrameMs :: Word32
targetFrameMs = 1000 `div` targetFps

appLoop :: Renderer -> Texture -> IO ()
appLoop renderer texture = do
    frameStart <- getTicks
    events <- pollEvents
    let eventIsExitPress event = case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          WindowClosedEvent _ -> True
          _ -> False
        exitPressed = any eventIsExitPress events
    renderFrame renderer texture
    frameEnd <- getTicks
    let elapsed = frameEnd - frameStart
    delay (targetFrameMs - elapsed)
    unless exitPressed (appLoop renderer texture)

