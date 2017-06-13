module HW2Spec (spec) where

import HW2
import Log
import Test.Hspec

spec :: Spec
spec = describe "parseMessage" $ do
    describe "info" $ do
      it "works" $
        parseMessage "I 147 mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` LogMessage Info 147 "mice in the air, I’m afraid, but you might catch a bat, and"
      it "handles bad timestamp" $
        parseMessage "I xxxx mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` Unknown "I xxxx mice in the air, I’m afraid, but you might catch a bat, and"
    describe "error" $ do
      it "works" $
        parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      it "handles bad timestamp" $
        parseMessage "E 404 xxxx mice in the air, I’m afraid, but you might catch a bat, and" `shouldBe` Unknown "E 404 xxxx mice in the air, I’m afraid, but you might catch a bat, and"
    describe "unknown" $
      it "passes string through" $
        parseMessage "Whatever" `shouldBe` Unknown "Whatever"

