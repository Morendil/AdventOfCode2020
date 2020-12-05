import Data.List (sort)

seatId :: String -> Int
seatId passport = (row * 8) + col
  where row = unbinary $ bdigits ('F','B') $ take 7 passport
        col = unbinary $ bdigits ('L','R') $ drop 7 passport

unbinary :: [Int] -> Int
unbinary = foldl (\bit acc -> (bit*2) + acc) 0

bdigits :: (Char, Char) -> String -> [Int]
bdigits (lo, hi) s = map repl s
  where repl c | c == lo = 0
        repl c | c == hi = 1
        repl _ = error "Only two digits"

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
