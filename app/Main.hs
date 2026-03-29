module Main where

import SDL
import qualified SDL.Font as FONT
import qualified Data.Text as Text
import Control.Monad (unless)
import SDL.Raw (getTicks)
import Data.Word (Word32)
import Foreign.C (CInt)

import Snake

import Assets
import Graphics

main :: IO ()
main = do
    initializeAll
    FONT.initialize
    window <- createWindow (Text.pack "Haskell Snake") defaultWindow {windowInitialSize = snakeWindowSize}
    renderer <- createRenderer window (-1) defaultRenderer
    
    assets <- loadAssets renderer
    appLoop renderer assets (initialState boardSize (2, 2))
    freeAssets assets
    destroyRenderer renderer
    destroyWindow window

targetFps :: Word32
targetFps = 5

targetFrameMs :: Word32
targetFrameMs = 1000 `div` targetFps

boardSize :: (Int, Int)
boardSize = (12, 8)

boardWindowSize :: (Int, Int) -> V2 CInt
boardWindowSize (w, h) = V2 (fromIntegral w * tileSize) (fromIntegral h * tileSize)

snakeWindowSize :: V2 CInt
snakeWindowSize = boardWindowSize boardSize

appLoop :: Renderer -> Assets -> Game -> IO ()
appLoop renderer assets game = do
    frameStart <- getTicks
    events <- pollEvents
    let eventIsExitPress event = case eventPayload event of
            KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            WindowClosedEvent _ -> True
            _ -> False
        exitPressed = any eventIsExitPress events
    let eventIsRestartPress event = case eventPayload event of
            KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeR
            _ -> False
        restartPresssed = any eventIsRestartPress events
    let eventDirectionPress event = case eventPayload event of
          KeyboardEvent keyboardEvent -> 
            if keyboardEventKeyMotion keyboardEvent == Pressed 
                then getInputByKeycode $ keysymKeycode (keyboardEventKeysym keyboardEvent) 
                else NothingPressed
          _ -> NothingPressed
        input = foldMap eventDirectionPress events
    (_, updatedGame) <- if restartPresssed then pure (IntoEmpty, initialState (10, 8) (2, 2)) else runTick game input
    renderFrame renderer assets updatedGame
    
    unless exitPressed $ do 
        frameEnd <- getTicks
        let elapsed = frameEnd - frameStart
        delay (targetFrameMs - elapsed)
        appLoop renderer assets updatedGame

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
