import Data.List (sort)

seatId :: String -> Int
seatId = unbinary . map (fromEnum . flip elem "BR")

unbinary :: [Int] -> Int
unbinary = foldl (\acc bit -> (acc*2) + bit) 0

findGap :: [Int] -> Int
findGap = (1+) . fst . head . dropWhile (\(x,y)->y==x+1) . pairs
  where pairs l = zip l (tail l)

main = do
  contents <- readFile "Day05.txt"
  let passes = lines contents
  -- part 1
  putStrLn $ show $ maximum $ map seatId passes
  -- part 2
  putStrLn $ show $ findGap $ sort $ map seatId passes
