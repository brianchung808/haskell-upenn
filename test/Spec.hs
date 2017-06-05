import Test.QuickCheck
import Lib

prop_toDigitsRev :: Int -> Bool
prop_toDigitsRev xs = (toDigitsRev xs) == (reverse (toDigits xs))

main :: IO ()
main = quickCheck prop_toDigitsRev
