import Data.Char (isAlphaNum)

data Spec = Spec Int Int Char String deriving Show

convert :: [String] -> Spec
convert [minl, maxl, k, p] = Spec (read minl) (read maxl) (head k) p
convert _ = error "No parse"

parse :: String -> Spec
parse = convert . words . map eachChar
  where eachChar c | isAlphaNum c = c
        eachChar _ = ' '

validPart1 :: Spec -> Bool
validPart1 spec = count >= minl && count <= maxl
  where (Spec minl maxl k p) = spec
        count = length $ filter (== k) p

validPart2 :: Spec -> Bool
validPart2 spec = atOne /= atTwo
  where (Spec one two k p) = spec
        atOne = (p !! (one-1)) == k
        atTwo = (p !! (two-1)) == k

main = do
  contents <- readFile "Day01.txt"
  let entries = lines contents
  let specs = map parse entries
  let ok = length $ filter validPart2 specs
  putStrLn $ show ok
