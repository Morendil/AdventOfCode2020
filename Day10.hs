import Data.List

main = do
  contents <- readFile "Day10.txt"
  let entries = map (read :: String -> Integer) $ lines contents
  let sorted = sort entries
  let differences = (zipWith (-) sorted (0:sorted)) ++ [3]
  let count n l = length $ filter (==n) l
  -- part 1
  print $ (count 1 differences) * (count 3 differences)
  print $ product $ map (\n -> toInteger . length $ arrangements [1..n+1]) $ map (toInteger .length) $ filter (\l -> head l == 1 && length l > 1) $ group differences

arrangements :: [Integer] -> [[Integer]]
arrangements [x] = [[x]]
arrangements (x:xs) = map (\path -> x:path) $ concatMap arrangements paths
  where nexts = takeWhile (<=x+3) xs
        paths = map (flip drop xs) [0..length nexts-1]
