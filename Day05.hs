import Data.List (sort)

seatId :: String -> Int
seatId = unbinary . map (fromEnum . flip elem "BR")

unbinary :: [Int] -> Int
unbinary = foldl (\acc bit -> (acc*2) + bit) 0

findGap :: [Int] -> Int
findGap l = 1 + head l + (length $ takeWhile (==1) $ steps)
  where steps = zipWith (-) (drop 1 l) l

main = do
  contents <- readFile "Day05.txt"
  let passes = lines contents
  -- part 1
  putStrLn $ show $ maximum $ map seatId passes
  -- part 2
  putStrLn $ show $ findGap $ sort $ map seatId passes
