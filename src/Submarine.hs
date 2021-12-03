module Submarine where

data Command
  = Forward Int
  | Down Int
  | Up Int

data Position = Position { horizontal :: Int, depth :: Int } deriving (Show, Eq)
data PositionWithAim = PositionWithAim { horizontal :: Int, depth :: Int, aim :: Int } deriving (Show, Eq)

navigate :: [Command] -> Position
navigate = foldl navigate' startPosition

navigate' :: Position -> Command -> Position
navigate' (Position h d) (Forward n) = Position (h + n) d
navigate' (Position h d) (Down n)    = Position h (d + n)
navigate' (Position h d) (Up n)      = Position h (d - n)

startPosition :: Position
startPosition = Position 0 0

navigateWithAim :: [Command] -> PositionWithAim
navigateWithAim = foldl navigateWithAim' startPositionWithAim

startPositionWithAim :: PositionWithAim
startPositionWithAim = PositionWithAim 0 0 0

navigateWithAim' :: PositionWithAim -> Command -> PositionWithAim
navigateWithAim' (PositionWithAim h d a) (Down n) =
  PositionWithAim h d (a + n)
navigateWithAim' (PositionWithAim h d a) (Up n) =
  PositionWithAim h d (a - n)
navigateWithAim' (PositionWithAim h d a) (Forward n) =
  PositionWithAim (h + n) (d + (a * n)) a
