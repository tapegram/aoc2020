module Day1Part1Spec where
import           Day1PuzzleInput  (day1puzzleInput)
import           Sonar            (countIncreasing)
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe)

-- https://adventofcode.com/2021/day/1

-- --- Day 1: Sonar Sweep ---
-- You're minding your own business on a ship at sea when the overboard alarm goes off! You rush to see if you can help. Apparently, one of the Elves tripped and accidentally sent the sleigh keys flying into the ocean!
-- Before you know it, you're inside a submarine the Elves keep ready for situations like this. It's covered in Christmas lights (because of course it is), and it even has an experimental antenna that should be able to track the keys if you can boost its signal strength high enough; there's a little meter that indicates the antenna's signal strength by displaying 0-50 stars.
-- Your instincts tell you that in order to save Christmas, you'll need to get all fifty stars by December 25th.
-- Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
-- As the submarine drops below the surface of the ocean, it automatically performs a sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report (your puzzle input) appears: each line is a measurement of the sea floor depth as the sweep looks further and further away from the submarine.
-- For example, suppose you had the following report:
-- 199
-- 200
-- 208
-- 210
-- 200
-- 207
-- 240
-- 269
-- 260
-- 263
-- This report indicates that, scanning outward from the submarine, the sonar sweep found depths of 199, 200, 208, 210, and so on.
-- The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.
-- To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:
-- 199 (N/A - no previous measurement)
-- 200 (increased)
-- 208 (increased)
-- 210 (increased)
-- 200 (decreased)
-- 207 (increased)
-- 240 (increased)
-- 269 (increased)
-- 260 (decreased)
-- 263 (increased)
-- In this example, there are 7 measurements that are larger than the previous measurement.
-- How many measurements are larger than the previous measurement?

spec_prelude :: Spec
spec_prelude = describe "SonarSweep.countIncreasing" $ do
  it "should return 0 for an empty list" $ do
    countIncreasing [] `shouldBe` 0
  it "should return 0 for a list with a single element" $ do
    countIncreasing [1] `shouldBe` 0
    countIncreasing [0] `shouldBe` 0
    countIncreasing [200] `shouldBe` 0
  it "should return 1 for a two element list that increases" $ do
    countIncreasing [1, 2] `shouldBe` 1
    countIncreasing [0, 100] `shouldBe` 1
    countIncreasing [200, 201] `shouldBe` 1
  it "should return 0 for a two element list that decreases" $ do
    countIncreasing [2, 1] `shouldBe` 0
    countIncreasing [100, 0] `shouldBe` 0
    countIncreasing [200, 199] `shouldBe` 0
  it "should correctly count only increases in a list with increasing and decreasing steps" $ do
    countIncreasing [0, 1, 0, 2, 1, 0] `shouldBe` 2
    countIncreasing [0, 1, 2, 3, 1, 0] `shouldBe` 3
  it "should return 0 for the same value" $ do
    countIncreasing [1, 1] `shouldBe` 0
  it "should work with the example case" $ do
    countIncreasing [
      199,
      200,
      208,
      210,
      200,
      207,
      240,
      269,
      260,
      263] `shouldBe` 7
  it "should work with the actual question data" $ do
    countIncreasing day1puzzleInput `shouldBe` 1482
