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

    appLoop renderer texture (initialState (10, 10) (2, 2))

    destroyTexture texture
    destroyRenderer renderer
    destroyWindow window

targetFps :: Word32
targetFps = 5

targetFrameMs :: Word32
targetFrameMs = 1000 `div` targetFps

appLoop :: Renderer -> Texture -> Game -> IO ()
appLoop renderer texture game = do
    frameStart <- getTicks
    events <- pollEvents
    let eventIsExitPress event = case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          WindowClosedEvent _ -> True
          _ -> False
        exitPressed = any eventIsExitPress events
    let eventDirectionPressed event = case eventPayload event of
          KeyboardEvent keyboardEvent -> 
            if keyboardEventKeyMotion keyboardEvent == Pressed 
                then getInputByKeycode $ keysymKeycode (keyboardEventKeysym keyboardEvent) 
                else NothingPressed
          _ -> NothingPressed
        input = foldMap eventDirectionPressed events
    frameEnd <- getTicks
    let elapsed = frameEnd - frameStart
    (_, updatedGame) <- runTick game input
    renderFrame renderer texture updatedGame
    
    unless exitPressed $ do 
        delay (targetFrameMs - elapsed)
        appLoop renderer texture updatedGame

getInputByKeycode :: Keycode -> MovementInput
getInputByKeycode KeycodeW = UpPressed
getInputByKeycode KeycodeUp = UpPressed
getInputByKeycode KeycodeD = RightPressed
getInputByKeycode KeycodeRight = RightPressed
getInputByKeycode KeycodeS = DownPressed
getInputByKeycode KeycodeDown = DownPressed
getInputByKeycode KeycodeA = LeftPressed
getInputByKeycode KeycodeLeft = LeftPressed
getInputByKeycode _ = NothingPressed
