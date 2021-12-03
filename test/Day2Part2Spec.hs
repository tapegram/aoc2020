module Day2Part2Spec where

import           Day2PuzzleInput  (day2PuzzleInput)
import           Submarine
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe)

-- https://adventofcode.com/2021/day/2
--- Part Two ---
-- Based on your calculations, the planned course doesn't seem to make any sense. You find the submarine manual and discover that the process is actually slightly more complicated.

-- In addition to horizontal position and depth, you'll also need to track a third value, aim, which also starts at 0. The commands also mean something entirely different than you first thought:

-- down X increases your aim by X units.
-- up X decreases your aim by X units.
-- forward X does two things:
-- It increases your horizontal position by X units.
-- It increases your depth by your aim multiplied by X.
-- Again note that since you're on a submarine, down and up do the opposite of what you might expect: "down" means aiming in the positive direction.

-- Now, the above example does something different:

-- forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
-- down 5 adds 5 to your aim, resulting in a value of 5.
-- forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
-- up 3 decreases your aim by 3, resulting in a value of 2.
-- down 8 adds 8 to your aim, resulting in a value of 10.
-- forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
-- After following these new instructions, you would have a horizontal position of 15 and a depth of 60. (Multiplying these produces 900.)

-- Using this new interpretation of the commands, calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?

spec_prelude :: Spec
spec_prelude = describe "Submarine.navigateWithAim" $ do
  it "should return 0,0,0 for an empty list" $ do
    navigateWithAim [] `shouldBe` PositionWithAim 0 0 0
  it "can go forward" $ do
    navigateWithAim [Forward 1] `shouldBe` PositionWithAim 1 0 0
    navigateWithAim [Down 1, Forward 1] `shouldBe` PositionWithAim 1 1 1
  it "can go down" $ do
    navigateWithAim [Down 1] `shouldBe` PositionWithAim 0 0 1
    navigateWithAim [Down 1, Down 2] `shouldBe` PositionWithAim 0 0 3
  it "can go up" $ do
    navigateWithAim [Up 1] `shouldBe` PositionWithAim 0 0 (-1)
    navigateWithAim [Up 1, Up 2] `shouldBe` PositionWithAim 0 0 (-3)
  it "can do the example from the promt" $ do
    navigateWithAim [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2] `shouldBe` PositionWithAim 15 60 10
  it "can solve the problem for the given input" $ do
    input <- day2PuzzleInput
    navigateWithAim input `shouldBe` PositionWithAim 1890 986622 1172
