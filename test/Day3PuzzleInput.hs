module Day3PuzzleInput where

day3PuzzleInput :: IO [String]
day3PuzzleInput = lines <$> readFile "./test/Day3PuzzleInput.txt"
