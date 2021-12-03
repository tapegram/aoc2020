module Day2PuzzleInput where

import           Data.List.Split
import           Submarine

parseInt :: String -> Int
parseInt = read

day2PuzzleInput :: IO [Command]
day2PuzzleInput = parseCommands . lines <$> readFile "./test/Day2PuzzleInput.txt"

parseCommands :: [String] -> [Command]
parseCommands = fmap parseCommand

-- Lying here about this always succeeding, etc.

parseCommand :: String -> Command
parseCommand s = toCommand $ splitOn " " s

toCommand :: [String] -> Command
toCommand ["forward", n] = Forward $ parseInt n
toCommand ["down", n]    = Down $ parseInt n
toCommand ["up", n]      = Up $ parseInt n
toCommand _              = error "Whoopsie"
