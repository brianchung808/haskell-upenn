module HW2Spec (spec) where

import HW2
import Log
import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    describe "info" $
      it "works" $
        parseMessage "I 147 mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` (LogMessage Info 147 "mice in the air, I’m afraid, but you might catch a bat, and")
    describe "error" $
      it "works" $
        parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
    describe "unknown" $
      it "passes string through" $
        parseMessage "Whatever" `shouldBe` Unknown "Whatever"

