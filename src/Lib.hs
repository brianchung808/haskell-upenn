module Lib where

import Data.Char (digitToInt)
import Prelude

toDigits :: Int -> [Int]
toDigits n
  | n > 0 = map digitToInt . show $ n
  | otherwise = []

toDigitsRev :: Int -> [Int]
toDigitsRev = reverse . toDigits
