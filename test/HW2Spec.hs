module HW2Spec (spec) where

import Test.QuickCheck
import HW2
import Log
import Test.Hspec

spec :: Spec
spec = describe "parseMessage" $ do
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


escape :: String -> String
escape = unwords . words

genInfo :: Gen LogMessage
genInfo = do
  a <- arbitrary
  b <- arbitrary
  return (LogMessage Info a (escape b))


genWarning :: Gen LogMessage
genWarning = do
  a <- arbitrary
  b <- arbitrary
  return (LogMessage Warning a (escape b))

genError :: Gen LogMessage
genError = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (LogMessage (Error a) b (escape c))

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

