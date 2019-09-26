module P3LongestSubstringSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import P3LongestSubstring

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "P3LongestSubstring" $ do
    it "handles []" $ do
      longestSubstring "" `shouldBe` 0

    it "handles one char" $ do
      longestSubstring "a" `shouldBe` 1
    
    it "handles more chars" $ do
      longestSubstring "aa" `shouldBe` 1
      longestSubstring "ab" `shouldBe` 2
      longestSubstring "abb" `shouldBe` 2
      longestSubstring "abc" `shouldBe` 3
      longestSubstring "abca" `shouldBe` 3
      longestSubstring "aabc" `shouldBe` 3

    it "handle complicate" $ do
      longestSubstring "abcbcdec" `shouldBe` 4

    it "handle leetcode" $ do
      longestSubstring "abcabcbb" `shouldBe` 3
      longestSubstring "bbbbb" `shouldBe` 1
      longestSubstring "pwwkew" `shouldBe` 3
