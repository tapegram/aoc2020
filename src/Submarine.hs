module Submarine where

data Command
  = Forward Int
  | Down Int
  | Up Int

data Position = Position { horizontal :: Int, depth :: Int } deriving (Show, Eq)

navigate :: [Command] -> Position
navigate = foldl navigate' startPosition

navigate' :: Position -> Command -> Position
navigate' (Position h d) (Forward n) = Position (h + n) d
navigate' (Position h d) (Down n)    = Position h (d + n)
navigate' (Position h d) (Up n)      = Position h (d - n)

startPosition :: Position
startPosition = Position 0 0
