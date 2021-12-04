module Diagnostic where
import           Data.Bits (Bits (bit, testBit, (.&.)))
import           Data.Char (digitToInt)

-- Assuming all numbers are the same length
-- Otherwise this breaks down. But this should work for any length
-- (within reasonable bounds) as long as all input are the same length.
-- Not explicitly checking for this because :shrug:.

type DiagnosticReport = [[Bit]]
powerConsumption :: DiagnosticReport -> Int
powerConsumption report =
  g * e
  where verticals = rotate report
        g = toInt $ gamma verticals
        e = toInt $ epsilon verticals

lifeSupportRating :: DiagnosticReport -> Int
lifeSupportRating report =
  oxygen * co2scrubbing
  where oxygen = oxygenGeneratorRating report
        co2scrubbing = co2ScrubberRating report

oxygenGeneratorRating :: DiagnosticReport -> Int
oxygenGeneratorRating report = oxygenGeneratorRating' report 0

oxygenGeneratorRating' :: DiagnosticReport -> Index -> Int
oxygenGeneratorRating' [] _ = 0
oxygenGeneratorRating' [x] _ = toInt x
oxygenGeneratorRating' report index =
  oxygenGeneratorRating' (filterDiagnosticValues bit index report) (index + 1)
  where
        vertical :: [Bit]
        vertical = foldl (\acc row -> acc ++ [row !! index]) [] report
        bit :: Bit
        bit = if isMostCommon One vertical then One else Zero

co2ScrubberRating :: DiagnosticReport -> Int
co2ScrubberRating report = co2ScrubberRating' report 0

co2ScrubberRating' :: DiagnosticReport -> Index -> Int
co2ScrubberRating' [] _ = 0
co2ScrubberRating' [x] _ = toInt x
co2ScrubberRating' report index =
  co2ScrubberRating' (filterDiagnosticValues bit index report) (index + 1)
  where
        vertical :: [Bit]
        vertical = foldl (\acc row -> acc ++ [row !! index]) [] report
        bit :: Bit
        bit = if isLeastCommon Zero vertical then Zero else One

type Index = Int
filterDiagnosticValues :: Bit -> Index -> DiagnosticReport -> DiagnosticReport
filterDiagnosticValues bit index = filter (\row -> (row !! index) == bit)

rotate :: [[a]] -> [[a]]
rotate []      = []
rotate ([]:xs) = rotate xs
rotate xs      = takeFirstFromAll xs : rotate (dropFirstFromAll xs)

takeFirstFromAll :: [[a]] -> [a]
takeFirstFromAll = foldl takeAndAppend mempty
  where takeAndAppend acc x = acc <> take 1 x

dropFirstFromAll :: [[a]] -> [[a]]
dropFirstFromAll = fmap (drop 1)

data Bit = Zero | One deriving (Eq, Show)

mostCommonBit :: [Bit] -> Bit
mostCommonBit bits = if isMostCommon One bits then One else Zero

isMostCommon :: Bit -> [Bit] -> Bool
isMostCommon bit bits =
  countMatching (== bit) bits >= countMatching (== flipBit bit) bits

isLeastCommon :: Bit -> [Bit] -> Bool
isLeastCommon bit bits =
  countMatching (== bit) bits <= countMatching (== flipBit bit) bits

countMatching :: (a -> Bool) -> [a] -> Int
countMatching predicate = length . filter predicate

toBit :: Char -> Bit
toBit '0' = Zero
toBit _   = One

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

stringsToBits :: [String] -> [[Bit]]
stringsToBits = fmap (fmap toBit)

gamma :: [[Bit]] -> [Bit]
gamma = fmap mostCommonBit

epsilon :: [[Bit]] -> [Bit]
epsilon = fmap (flipBit . mostCommonBit)

toInt :: [Bit] -> Int
toInt  = foldl (\acc x -> acc * 2 + toInt' x) 0

toInt' :: Bit -> Int
toInt' Zero = 0
toInt' One  = 1
