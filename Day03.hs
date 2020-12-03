main :: IO ()
main = do
  contents <- readFile "Day01.txt"
  let area = lines contents
  putStrLn $ show $ foldr (*) 1 [countTrees 1 1 area, countTrees 3 1 area, countTrees 5 1 area, countTrees 7 1 area, countTrees 1 2 area]

countTrees :: Int -> Int -> [String] -> Int
countTrees right down area = length $ filter (== '#') $ trees right down area

trees :: Int -> Int -> [String] -> String
trees right down area = zipWith (!!) (map cycle path) x_indices
  where path = every down area
        x_indices = [n*right|n<-[0..(length area)-1]]

every :: Int -> [a] -> [a]
every n [] = []
every n (x:xs) = x : every n (drop (n-1) xs)
