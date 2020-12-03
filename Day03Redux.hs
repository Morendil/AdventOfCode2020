main = do
  contents <- readFile "sample.txt"
  let area = lines contents
  putStrLn $ show $ countTrees 1 3 area
  putStrLn $ show $ foldr (*) 1 $ [countTrees 1 1 area, countTrees 1 3 area, countTrees 1 5 area, countTrees 1 7 area, countTrees 2 1 area]

countTrees :: Int -> Int -> [String] -> Int
countTrees down right area = length $ filter (== '#') $ path down right area

path :: Int -> Int -> [String] -> String
path down right area = map ((flip at) area) $ indices down right area

indices :: Int -> Int -> [String] -> [(Int, Int)]
indices down right area = zip x_indices y_indices
  where x_indices = [(x*right) `mod` width|x<-[0..]]
        y_indices = takeWhile (<=height) [(y*down)|y<-[0..]]
        height = length area - 1
        width = length (head area)

at :: (Int, Int) -> [String] -> Char
at (x, y) lines = (lines !! y) !! x
