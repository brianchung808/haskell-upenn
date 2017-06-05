module Lib where

import Data.Char (digitToInt)
import Prelude

-- Exercise 1

toDigits :: Int -> [Int]
toDigits n
  | n > 0 = map digitToInt . show $ n
  | otherwise = []

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits

-- Exercise 2
doubleEveryOther :: [Int] -> [Int]
-- doubleEveryOther [] = []
-- doubleEveryOther (x:[]) = [x]
-- doubleEveryOther l = (rest ++ [2*y]) ++ [x]
--   where (x:y:xs) = reverse l
--         rest = doubleEveryOther . reverse $ xs
doubleEveryOther l = reverse $ zipWith (\x -> \y -> y x) xs $ take (length xs) $ cycle [\x -> x, (* 2)]
  where xs = reverse l

-- Exercise 3
sumDigits :: [Int] -> Int
sumDigits = (foldl (+) 0) . (>>= toDigits)

-- Exercise 4
validate :: Int -> Bool
validate n = (rem (sumDigits . doubleEveryOther . toDigits $ n) 10) == 0


-- Exercise 4
type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

