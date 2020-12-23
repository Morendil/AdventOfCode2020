import Data.List
import Data.Maybe
import Data.Char

shuffle :: [Int] -> [Int]
shuffle cups = take len $ drop (next+1) $ cycle shuffled
  where left = head cups
        len = length cups
        right = take (len-4) $ drop 4 cups
        taken = take 3 $ tail cups
        remain = left:right
        target = left-1
        picked = pick target remain
        dest = fromJust $ elemIndex picked remain
        insert n l1 l2 = (take (n+1) l1) ++ l2 ++ (drop (n+1) l1)
        shuffled = insert dest remain taken
        next = fromJust $ elemIndex left shuffled

pick :: Int -> [Int] -> Int
pick target remain | target `elem` remain = target
pick target remain | target < 0 = maximum remain        
pick target remain = pick (target-1) remain

settle :: [Int] -> [Int]
settle cups = take 8 $ drop (one+1) $ cycle cups
  where one = fromJust $ elemIndex 1 cups

part1 :: String
part1 = map intToDigit $ settle $ last $ take 101 $ iterate shuffle [2,8,4,5,7,3,9,6,1]

main = do
  -- print $ map intToDigit $ settle $ last $ take 11 $ iterate shuffle [3,8,9,1,2,5,4,6,7] -- 92658374
  -- print $ map intToDigit $ settle $ last $ take 101 $ iterate shuffle [3,8,9,1,2,5,4,6,7] -- 67384529
  -- part1
  print $ part1
