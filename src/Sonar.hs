module Sonar (countIncreasing, countIncreasingWithSlidingWindow) where

countIncreasing :: [Int] -> Int
countIncreasing (prev:curr:rest) =
  if curr > prev
    then 1 + countIncreasing (curr:rest)
    else countIncreasing (curr:rest)
countIncreasing _ = 0

countIncreasingWithSlidingWindow :: [Int] -> Int
countIncreasingWithSlidingWindow =  countIncreasingWithSlidingWindow' Nothing

countIncreasingWithSlidingWindow' :: Maybe Int -> [Int] -> Int
countIncreasingWithSlidingWindow' Nothing (a: b: c: rest) = countIncreasingWithSlidingWindow' (Just $ a + b + c) (b: c: rest)
countIncreasingWithSlidingWindow' (Just prev) (a: b: c: rest) =
  if curr > prev
    then 1 + countIncreasingWithSlidingWindow' (Just curr) (b: c: rest)
    else countIncreasingWithSlidingWindow' (Just curr) (b: c: rest)
  where curr = a + b + c

countIncreasingWithSlidingWindow' _ _                         = 0
