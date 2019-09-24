module P1TwoSumSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import P1TwoSum

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "P1TwoSum" $ do
    it "works" $ do
      twoSum [2, 7, 11, 15] 9 `shouldBe` Just (0, 1)
