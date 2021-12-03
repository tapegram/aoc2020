module Day2Part1Spec where

import           Day2PuzzleInput  (day2PuzzleInput)
import           Submarine
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe)

-- https://adventofcode.com/2021/day/2
--- Day 2: Dive! ---
-- Now, you need to figure out how to pilot this thing.

-- It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:

-- forward X increases the horizontal position by X units.
-- down X increases the depth by X units.
-- up X decreases the depth by X units.
-- Note that since you're on a submarine, down and up affect your depth, and so they have the opposite result of what you might expect.

-- The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's going. For example:

-- forward 5
-- down 5
-- forward 8
-- up 3
-- down 8
-- forward 2
-- Your horizontal position and depth both start at 0. The steps above would then modify them as follows:

-- forward 5 adds 5 to your horizontal position, a total of 5.
-- down 5 adds 5 to your depth, resulting in a value of 5.
-- forward 8 adds 8 to your horizontal position, a total of 13.
-- up 3 decreases your depth by 3, resulting in a value of 2.
-- down 8 adds 8 to your depth, resulting in a value of 10.
-- forward 2 adds 2 to your horizontal position, a total of 15.
-- After following these instructions, you would have a horizontal position of 15 and a depth of 10. (Multiplying these together produces 150.)

-- Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?

spec_prelude :: Spec
spec_prelude = describe "Submarine.navigate" $ do
  it "should return 0,0 for an empty list" $ do
    navigate [] `shouldBe` Position 0 0
  it "can go forward" $ do
    navigate [Forward 1] `shouldBe` Position 1 0
    navigate [Forward 1, Forward 2] `shouldBe` Position 3 0
  it "can go down" $ do
    navigate [Down 1] `shouldBe` Position 0 1
    navigate [Down 1, Down 2] `shouldBe` Position 0 3
  it "can go up" $ do
    navigate [Up 1] `shouldBe` Position 0 (-1)
    navigate [Up 1, Up 2] `shouldBe` Position 0 (-3)
  it "can do the example from the promt" $ do
    navigate [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2] `shouldBe` Position 15 10
  it "can solve the problem for the given input" $ do
    input <- day2PuzzleInput
    navigate input `shouldBe` Position 1890 1172
