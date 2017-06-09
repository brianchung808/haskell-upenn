module Lib where

import Data.Char (digitToInt)
import Prelude

toDigits :: Int -> [Int]
toDigits n
  | n > 0 = map digitToInt . show $ n
  | otherwise = []

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther l = rest ++ [2*y] ++ [x]
  where (x:y:xs) = reverse l
        rest = doubleEveryOther . reverse $ xs

-- doubleEveryOther l = reverse $ zipWith apply xs $ take (length xs) $ ops
--   where xs = reverse l
--         apply = (\x -> \y -> y x)
--         ops = cycle [\x -> x, (* 2)]

-- instance Monoid Int where
--   mempty  = 0
--   mappend = (+)

sumDigits :: [Int] -> Int
sumDigits = sum . (>>= toDigits)
-- sumDigits = mconcat . (>>= toDigits)
-- sumDigits = (foldl mappend mempty) . (>>= toDigits)
-- sumDigits = (foldl (+) 0) . (>>= toDigits)

validate :: Int -> Bool
validate n = rem (lunh n) 10 == 0

lunh :: Int -> Int
lunh = sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
