module P2AddTwoNumbersSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import P2AddTwoNumbers

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "P2AddTwoNumbers" $ do
    it "works for equal length numbers" $ do
      addTwoNumbers [2,4,3] [5,6,4] `shouldBe` [7,0,8]

    it "works for difference length numbers" $ do
      addTwoNumbers [2,4,3] [5,6] `shouldBe` [7,0,4]

    it "works for 99" $ do
      addTwoNumbers [2,4,3] [9,9] `shouldBe` [1,4,4]

    it "works for 0" $ do
      addTwoNumbers [2,4,3] [0] `shouldBe` [2,4,3]

    it "works for 0s" $ do
      addTwoNumbers [0] [0] `shouldBe` [0]
