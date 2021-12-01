module Sonar where

countIncreasing :: [Int] -> Int
countIncreasing (prev:curr:rest) =
  if curr > prev
    then 1 + countIncreasing (curr:rest)
    else countIncreasing (curr:rest)
countIncreasing _ = 0
