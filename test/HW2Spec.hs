module HW2Spec (spec) where

import Test.QuickCheck
import HW2
import Log
import Test.Hspec
import Data.List (intercalate)

spec :: Spec
spec = do
  describe "parseMessage" $ do
    describe "info" $ do
      it "works" $ property $
        forAll genInfo $ \x -> parseMessage (showLogMessage x) === x
      it "handles bad timestamp" $
        parseMessage "I xxxx mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` Unknown "I xxxx mice in the air, I’m afraid, but you might catch a bat, and"

    describe "error" $ do
      it "works" $
        forAll genError $ \x -> parseMessage (showLogMessage x) === x
      it "handles bad timestamp" $
        parseMessage "E 404 xxxx mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` Unknown "E 404 xxxx mice in the air, I’m afraid, but you might catch a bat, and"

    describe "warning" $ do
      it "works" $ property $
        forAll genWarning $ \x -> parseMessage (showLogMessage x) === x
      it "handles bad timestamp" $
        parseMessage "W xxxx mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` Unknown "W xxxx mice in the air, I’m afraid, but you might catch a bat, and"

    describe "unknown" $
      it "passes string through" $ property $
        forAll genEscapedString $ \s -> parseMessage s === Unknown s

  describe "parse" $
    it "works" $
      (parse . intercalate "\n") ["I 6 Completed armadillo processing", "I 1 Nothing to report", "I 4 Everything normal", "I 11 Initiating self-destruct sequence", "E 70 3 Way too many pickles", "E 65 8 Bad pickle-flange interaction detected", "W 5 Flange is due for a check-up", "I 7 Out for lunch, back in two time steps", "E 20 2 Too many pickles", "I 9 Back from lunch", "E 99 10 Flange failed"]
      `shouldBe`
      [LogMessage Info 6 "Completed armadillo processing", LogMessage Info 1 "Nothing to report", LogMessage Info 4 "Everything normal", LogMessage Info 11 "Initiating self-destruct sequence", LogMessage (Error 70) 3 "Way too many pickles", LogMessage (Error 65) 8 "Bad pickle-flange interaction detected", LogMessage Warning 5 "Flange is due for a check-up", LogMessage Info 7 "Out for lunch, back in two time steps", LogMessage (Error 20) 2 "Too many pickles", LogMessage Info 9 "Back from lunch", LogMessage (Error 99) 10 "Flange failed"]

  describe "insert" $ do
    it "noops for Unknown" $ property $
      forAll genUnknown $ \x -> insert x Leaf === Leaf
    it "handles Leaf" $
      insert (LogMessage Info 6 "hi") Leaf `shouldBe` Node Leaf (LogMessage Info 6 "hi") Leaf
    it "can handle one greater" $
      insert (LogMessage Info 6 "there") (Node Leaf (LogMessage Info 5 "hi") Leaf) `shouldBe` Node Leaf (LogMessage Info 5 "hi") (Node Leaf (LogMessage Info 6 "there") Leaf)
    it "can handle one lesser" $
      insert (LogMessage Info 4 "there") (Node Leaf (LogMessage Info 5 "hi") Leaf) `shouldBe` Node (Node Leaf (LogMessage Info 4 "there") Leaf) (LogMessage Info 5 "hi") Leaf
    it "can handle one equal" $
      insert (LogMessage Info 5 "there") (Node Leaf (LogMessage Info 5 "hi") Leaf) `shouldBe` Node Leaf (LogMessage Info 5 "hi") (Node Leaf (LogMessage Info 5 "there") Leaf)

  describe "build" $ do
    it "is Leaf for empty list" $
      build [] `shouldBe` Leaf
    it "works" $
      (printOrder . build) [LogMessage Info 6 "Completed armadillo processing", LogMessage Info 1 "Nothing to report", LogMessage Info 4 "Everything normal", LogMessage Info 11 "Initiating self-destruct sequence", LogMessage (Error 70) 3 "Way too many pickles", LogMessage (Error 65) 8 "Bad pickle-flange interaction detected", LogMessage Warning 5 "Flange is due for a check-up", LogMessage Info 7 "Out for lunch, back in two time steps", LogMessage (Error 20) 2 "Too many pickles", LogMessage Info 9 "Back from lunch", LogMessage (Error 99) 10 "Flange failed"]
      `shouldBe`
      [1..11]
  describe "inOrder" $ do
    it "is empty for Leaf" $
      inOrder Leaf `shouldBe` []
    it "works" $ do
      (inOrder . build) [LogMessage Info 6 "Completed armadillo processing", LogMessage Info 1 "Nothing to report", LogMessage Info 4 "Everything normal", LogMessage Info 11 "Initiating self-destruct sequence", LogMessage (Error 70) 3 "Way too many pickles", LogMessage (Error 65) 8 "Bad pickle-flange interaction detected", LogMessage Warning 5 "Flange is due for a check-up", LogMessage Info 7 "Out for lunch, back in two time steps", LogMessage (Error 20) 2 "Too many pickles", LogMessage Info 9 "Back from lunch", LogMessage (Error 99) 10 "Flange failed"]
        `shouldBe`
        [LogMessage Info 1 "Nothing to report",LogMessage (Error 20) 2 "Too many pickles",LogMessage (Error 70) 3 "Way too many pickles",LogMessage Info 4 "Everything normal",LogMessage Warning 5 "Flange is due for a check-up",LogMessage Info 6 "Completed armadillo processing",LogMessage Info 7 "Out for lunch, back in two time steps",LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",LogMessage Info 9 "Back from lunch",LogMessage (Error 99) 10 "Flange failed",LogMessage Info 11 "Initiating self-destruct sequence"]

  describe "whatWentWrong" $ do
    it "works for all errors" $
      whatWentWrong [LogMessage (Error 3) 2 "Second Error", LogMessage (Error 3) 1 "First Error", LogMessage (Error 3) 3 "Third Error"] `shouldBe` ["First Error", "Second Error", "Third Error"]
    it "empty list for no errors" $ property $
      forAll (listOf genInfo) $ \x -> whatWentWrong x === []

printOrder :: MessageTree -> [Int]
printOrder Leaf = []
printOrder (Node m1 (LogMessage _ t _) m2) = printOrder m1 ++ [t] ++ printOrder m2

escape :: String -> String
escape = unwords . words

genInfo :: Gen LogMessage
genInfo = do
  a <- arbitrary
  b <- arbitrary
  return $ LogMessage Info a $ escape b

genUnknown :: Gen LogMessage
genUnknown = do
  a <- arbitrary
  return $ Unknown a

genWarning :: Gen LogMessage
genWarning = do
  a <- arbitrary
  b <- arbitrary
  return $ LogMessage Warning a $ escape b

genError :: Gen LogMessage
genError = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ LogMessage (Error a) b $ escape c

showLogMessage :: LogMessage -> String
showLogMessage (LogMessage Info t s) = "I " ++ show t ++ " " ++ s
showLogMessage (LogMessage Warning t s) = "W " ++ show t ++ " " ++ s
showLogMessage (LogMessage (Error e) t s) = "E " ++ show e ++ " " ++ show t ++ " " ++ s
showLogMessage (Unknown a) = "Unknown " ++ a

-- instance Show LogMessage where
--   show LogMessage Info t s = ""
--   show LogMessage (Error e) t s = ""
--   show _ = ""

genEscapedString :: Gen String
genEscapedString = do
  a <- arbitrary
  return (escape a)

