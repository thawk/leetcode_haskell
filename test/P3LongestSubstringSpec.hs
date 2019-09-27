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
      longestSubstring "abcbcdac" `shouldBe` 4

    it "handle leetcode" $ do
      longestSubstring "abcabcbb" `shouldBe` 3
      longestSubstring "bbbbb" `shouldBe` 1
      longestSubstring "pwwkew" `shouldBe` 3

  describe "P3LongestSubstring" $ do
    it "handles []" $ do
      longestSubstring' "" `shouldBe` (0, "")

    it "handles one char" $ do
      longestSubstring' "a" `shouldBe` (1, "a")
    
    it "handles more chars" $ do
      longestSubstring' "aa" `shouldBe` (1, "a")
      longestSubstring' "ab" `shouldBe` (2, "ab")
      longestSubstring' "abb" `shouldBe` (2, "ab")
      longestSubstring' "abc" `shouldBe` (3, "abc")
      longestSubstring' "abca" `shouldBe` (3, "abc")
      longestSubstring' "aabc" `shouldBe` (3, "abc")

    it "handle complicate" $ do
      longestSubstring' "abcbcdac" `shouldBe` (4, "bcda")

    it "handle leetcode" $ do
      longestSubstring' "abcabcbb" `shouldBe` (3, "abc")
      longestSubstring' "bbbbb" `shouldBe` (1, "b")
      longestSubstring' "pwwkew" `shouldBe` (3, "wke")
