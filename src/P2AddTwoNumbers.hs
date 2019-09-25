module P2AddTwoNumbers where


-- You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

-- You may assume the two numbers do not contain any leading zero, except the number 0 itself.

addTwoNumbers :: [Int] -> [Int] -> [Int]
addTwoNumbers x y = 
  add' xs ys 0
    where
      maxLen = max (length x) (length y)
      xs = x ++ take (maxLen - length x) (repeat 0)
      ys = y ++ take (maxLen - length y) (repeat 0)

      add' :: [Int] -> [Int] -> Int -> [Int]
      add' [] [] 0 = []
      add' [] [] i = [i]
      add' (x:xs) (y:ys) i = 
        r : add' xs ys q
        where
          (q, r) = (x + y + i) `quotRem` 10



