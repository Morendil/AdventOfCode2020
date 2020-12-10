import Data.List
import Data.Maybe
import qualified Data.Map as M

main = do
  contents <- readFile "sample1.txt"
  let entries = map (read :: String -> Integer) $ lines contents
  let sorted = sort entries
  let differences = (zipWith (-) sorted (0:sorted)) ++ [3]
  let count n l = length $ filter (==n) l
  -- part 1
  print $ (count 1 differences) * (count 3 differences)
  -- part 2
  print sorted
  print $ countArrangements sorted

countArrangements :: [Integer] -> Maybe Integer
countArrangements levels = M.lookup (last levels) $ foldl step (M.fromList [(0,1)]) levels
  where step state level = M.insert level (sum $ mapMaybe (flip M.lookup state) [level-1,level-2,level-3]) state
