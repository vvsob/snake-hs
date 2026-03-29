module Main (main) where

import Data.Array ((!))
import Snake
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "SnakeLib" [ growHeadTests, shrinkTailTests, runTickTests]

growHeadTests :: TestTree
growHeadTests =
    testGroup "growHead" [ 
        orientationCase "HEAD_UP + UP" HEAD_UP UP VERTICAL,
        orientationCase "HEAD_UP + LEFT" HEAD_UP LEFT TURN_DOWN_LEFT, 
        orientationCase "HEAD_UP + RIGHT" HEAD_UP RIGHT TURN_DOWN_RIGHT, 
        orientationCase "HEAD_RIGHT + RIGHT" HEAD_RIGHT RIGHT HORIZONTAL, 
        orientationCase "HEAD_RIGHT + UP" HEAD_RIGHT UP TURN_UP_LEFT, 
        orientationCase "HEAD_RIGHT + DOWN" HEAD_RIGHT DOWN TURN_DOWN_LEFT, 
        orientationCase "HEAD_DOWN + DOWN" HEAD_DOWN DOWN VERTICAL, 
        orientationCase "HEAD_DOWN + RIGHT" HEAD_DOWN RIGHT TURN_UP_RIGHT, 
        orientationCase "HEAD_DOWN + LEFT" HEAD_DOWN LEFT TURN_UP_LEFT, 
        orientationCase "HEAD_LEFT + LEFT" HEAD_LEFT LEFT HORIZONTAL, 
        orientationCase "HEAD_LEFT + DOWN" HEAD_LEFT DOWN TURN_DOWN_RIGHT, 
        orientationCase "HEAD_LEFT + UP" HEAD_LEFT UP TURN_UP_RIGHT
    ]
  where
    orientationCase label headOrientation direction expected =
        testCase label $
            assertEqual label expected (growHead headOrientation direction)

shrinkTailTests :: TestTree
shrinkTailTests =
    testGroup "shrinkTail" [ 
        orientationCase "HEAD_UP + UP" HEAD_UP UP TAIL_UP, 
        orientationCase "HEAD_RIGHT + RIGHT" HEAD_RIGHT RIGHT TAIL_RIGHT, 
        orientationCase "HEAD_DOWN + DOWN" HEAD_DOWN DOWN TAIL_DOWN, 
        orientationCase "HEAD_LEFT + LEFT" HEAD_LEFT LEFT TAIL_LEFT, 
        orientationCase "TURN_UP_RIGHT + LEFT" TURN_UP_RIGHT LEFT TAIL_UP, 
        orientationCase "TURN_UP_RIGHT + DOWN" TURN_UP_RIGHT DOWN TAIL_RIGHT, 
        orientationCase "TURN_DOWN_RIGHT + LEFT" TURN_DOWN_RIGHT LEFT TAIL_DOWN, 
        orientationCase "TURN_DOWN_RIGHT + UP" TURN_DOWN_RIGHT UP TAIL_RIGHT, 
        orientationCase "TURN_DOWN_LEFT + RIGHT" TURN_DOWN_LEFT RIGHT TAIL_DOWN, 
        orientationCase "TURN_DOWN_LEFT + UP" TURN_DOWN_LEFT UP TAIL_LEFT, 
        orientationCase "TURN_UP_LEFT + RIGHT" TURN_UP_LEFT RIGHT TAIL_UP, 
        orientationCase "TURN_UP_LEFT + DOWN" TURN_UP_LEFT DOWN TAIL_LEFT, 
        orientationCase "HORIZONTAL + RIGHT" HORIZONTAL RIGHT TAIL_RIGHT, 
        orientationCase "HORIZONTAL + LEFT" HORIZONTAL LEFT TAIL_LEFT, 
        orientationCase "VERTICAL + UP" VERTICAL UP TAIL_UP, 
        orientationCase "VERTICAL + DOWN" VERTICAL DOWN TAIL_DOWN
    ]
  where
    orientationCase label tailOrientation direction expected =
        testCase label $
            assertEqual label expected (shrinkTail tailOrientation direction)

runTickTests :: TestTree
runTickTests =
    testGroup "runTick" [ 
        testCase "initialState sets the starting snake and apples" $
            let game = initialState (10, 10) (3, 4)
                board = gameBoard game
             in do
                assertEqual "snakeHead" (3, 4) (snakeHead game)
                assertEqual "snakeTail" (2, 4) (snakeTail game)
                assertEqual "tail tile" (SnakeSegment TAIL_RIGHT) (board ! (2, 4))
                assertEqual "head tile" (SnakeSegment HEAD_RIGHT) (board ! (3, 4))
                assertEqual "first apple" Apple (board ! (5, 4))
                assertEqual "second apple" Apple (board ! (6, 4)),
        testCase "runTick with no input continues moving right" $
            let game = initialState (10, 10) (3, 4)
            in do
                (_, nextGame) <- runTick game NothingPressed
                let board = gameBoard nextGame
                assertEqual "snakeHead" (4, 4) (snakeHead nextGame)
                assertEqual "snakeTail" (3, 4) (snakeTail nextGame)
                assertEqual "new tail" (SnakeSegment TAIL_RIGHT) (board ! (3, 4))
                assertEqual "new head" (SnakeSegment HEAD_RIGHT) (board ! (4, 4))
                assertEqual "vacated tail" Empty (board ! (2, 4)),
        testCase "runTick ignores opposite-direction input" $
            let game = initialState (10, 10) (3, 4)
            in do
                (_, nextGame) <- runTick game LeftPressed
                let board = gameBoard nextGame
                assertEqual "snakeHead" (4, 4) (snakeHead nextGame)
                assertEqual "snakeTail" (3, 4) (snakeTail nextGame)
                assertEqual "new tail" (SnakeSegment TAIL_RIGHT) (board ! (3, 4))
                assertEqual "new head" (SnakeSegment HEAD_RIGHT) (board ! (4, 4)), 
        testCase "runTick turns the snake when given a valid direction" $
            let game = initialState (10, 10) (3, 4)
            in do
                (_, nextGame) <- runTick game DownPressed
                let board = gameBoard nextGame
                assertEqual "snakeHead" (3, 5) (snakeHead nextGame)
                assertEqual "snakeTail" (3, 4) (snakeTail nextGame)
                assertEqual "new tail after turning" (SnakeSegment TAIL_DOWN) (board ! (3, 4))
                assertEqual "new head" (SnakeSegment HEAD_DOWN) (board ! (3, 5))
                assertEqual "vacated tail" Empty (board ! (2, 4)), 
        testCase "runTick grows the snake after eating an apple" $
            let game = initialState (10, 10) (3, 4)
                initialSegments = snakeSegmentCount game
            in do
                (_, movedGame) <- runTick game NothingPressed
                (_, grownGame) <- runTick movedGame NothingPressed
                let board = gameBoard grownGame
                assertEqual "snakeHead" (5, 4) (snakeHead grownGame)
                assertEqual "snakeTail unchanged after growth" (3, 4) (snakeTail grownGame)
                assertEqual "segment count increased" (initialSegments + 1) (snakeSegmentCount grownGame)
                assertEqual "new body segment" (SnakeSegment HORIZONTAL) (board ! (4, 4))
                assertEqual "new head" (SnakeSegment HEAD_RIGHT) (board ! (5, 4)), 
        testCase "runTick wraps around the board edges" $
            let game = initialState (5, 5) (1, 3)
            in do
                (_, step1) <- runTick game UpPressed
                (_, step2) <- runTick step1 NothingPressed
                (_, step3) <- runTick step2 NothingPressed
                (_, step4) <- runTick step3 NothingPressed
                let board = gameBoard step4
                assertEqual "snakeHead" (1, 4) (snakeHead step4)
                assertEqual "wrapped head tile" (SnakeSegment HEAD_UP) (board ! (1, 4))
                assertEqual "tail advanced" (1, 0) (snakeTail step4)
    ]

