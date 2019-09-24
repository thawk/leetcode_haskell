module P1TwoSum where

-- Given an array of integers, return indices of the two numbers such that they add up to a specific target.
-- You may assume that each input would have exactly one solution, and you may not use the same element twice.

twoSum :: [Integer] -> Integer -> Maybe (Integer, Integer)
twoSum xs n = twoSum' (zip [0,1..] xs) n

twoSum' [] _ = Nothing
twoSum' (x:xs) n =
  case twoSum'' x xs n of 
    Just r  -> Just r
    Nothing ->  twoSum' xs n

twoSum'' _ [] _ = Nothing
twoSum'' (ix,x) ((iy,y):ys) n
  | x + y == n = Just (ix, iy)
  | otherwise  = twoSum'' (ix,x) ys n

