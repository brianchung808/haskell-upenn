module HW1Spec (spec) where

import Test.QuickCheck
import Test.Hspec
import HW1

genSinglePositive :: Gen Int
genSinglePositive = abs `fmap` (arbitrary :: Gen Int) `suchThat` \x -> (0 < x && x < 10)

genListSinglePositive :: Gen [Int]
genListSinglePositive = listOf genSinglePositive

genListSinglePositiveOddLength :: Gen [Int]
genListSinglePositiveOddLength = genListSinglePositive `suchThat` \xs -> rem (length xs) 2 /= 0

genListSinglePositiveEvenLength :: Gen [Int]
genListSinglePositiveEvenLength = genListSinglePositive `suchThat` \xs -> rem (length xs) 2 == 0

doubleEveryOtherOdd :: [Int] -> [Int]
doubleEveryOtherOdd xs = zipWith (\x -> \y -> y x) xs $ take (length xs) $ cycle [\x -> x, (* 2)]

doubleEveryOtherEven :: [Int] -> [Int]
doubleEveryOtherEven xs = zipWith (\x -> \y -> y x) xs $ take (length xs) $ cycle [(* 2), \x -> x]

spec :: Spec
spec = do
  describe "toDigitsRev" $ do
    it "is the reverse of toDigits" $ property $
      \xs -> toDigitsRev xs == (reverse . toDigits $ xs)

  describe "doubleEveryOther" $ do
    it "handles empty" $ do
      doubleEveryOther [] `shouldBe` []
    it "handles single" $ do
      forAll genSinglePositive $ \x -> doubleEveryOther [x] == [x]
    it "handles odd" $ do
      forAll genListSinglePositiveOddLength $ \x -> (doubleEveryOther x) == doubleEveryOtherOdd x
    it "handles even" $ do
      forAll genListSinglePositiveEvenLength $ \x -> (doubleEveryOther x) == doubleEveryOtherEven x

  describe "sumDigits" $ do
    it "can sum digits" $ do
      forAll genListSinglePositive $ \xs -> sumDigits xs == (foldl (+) 0 xs)
    it "handle empty" $ do
      sumDigits [] `shouldBe` 0
    it "handle single" $ property $
      forAll genSinglePositive $ \x -> sumDigits [x] == x

  describe "validate" $ do
    it "works on valid" $ do
      validate 4012888888881881 `shouldBe` True
    it "works on invalid" $ do
      validate 4012888888881882 `shouldBe` False

  describe "hanoi" $ do
    it "handles 1 disc" $ do
      hanoi 1 "a" "b" "c" == [("a", "b")]
    it "handles 2 disc" $ do
      hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
    it "handles 3 disc" $ do
      hanoi 3 "a" "b" "c" == [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
