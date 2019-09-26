module P3LongestSubstring where

import qualified Data.Map.Lazy as Map

-- Given a string, find the length of the longest substring without repeating characters.

longestSubstring :: [Char] -> Int
longestSubstring [] = 0
longestSubstring s = maximum ls
  where
    ls = map (\n -> longest Map.empty (drop n s) 0) [0..(length s - 1)]

    longest :: Map.Map Char Int -> [Char] -> Int -> Int
    longest _ [] l = l
    longest m (c:cs) l =
      case Map.lookup c m of
        Nothing   -> longest ( Map.insertWith (+) c 1 m ) cs ( l + 1 )
        otherwise -> l

  
