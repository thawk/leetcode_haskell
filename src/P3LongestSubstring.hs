module P3LongestSubstring where

import qualified Data.Map.Lazy as Map
import Control.Monad.State.Lazy (State, execState, get, put)
import Control.Monad (when)
import Data.List (elemIndex)

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

  
longestSubstring' :: [Char] -> (Int, String)
longestSubstring' s = (length longestStr, longestStr)
    where
      longestStr = execState (longest "" s) ""

      updateLongest curr = do
        ms <- get
        when (length curr >  length ms) (put curr)
        return ()

      longest :: String -> String -> State String ()
      longest curr [] = updateLongest curr

      longest curr (c:cs) =
        case elemIndex c curr of
          Just i  -> do
            updateLongest curr
            longest ((drop (i+1) curr) ++ [c]) cs
          Nothing ->
            longest (curr ++ [c]) cs


