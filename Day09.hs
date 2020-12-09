data Xmas = Xmas [Integer] [Integer]
  deriving (Eq, Show)

xmas :: Int -> [Integer] -> Xmas
xmas count entries = Xmas (take count entries) (drop count entries) 

valid :: Xmas -> Bool
valid xmas = any (\(a,b) -> a+b ==x) [(a,b)|a<-pre,b<-pre,a/=b]
  where Xmas pre (x:xs) = xmas

next :: Xmas -> Xmas
next (Xmas (p:ps) (x:xs)) = Xmas (ps++[x]) xs

part1 :: Int -> [Integer] -> Integer
part1 len entries = head result
  where start = xmas len entries
        final = next $ last $ takeWhile valid $ iterate next $ start
        Xmas _ result = final

givenSumSub :: Integer -> [Integer] -> [Integer]
givenSumSub target array = go 0 2 array
  where go lo len arr = if result == target then current
                          else if result < target then go lo (len+1) arr
                                  else go (lo+1) (len-1) arr
                        where current = take len $ drop lo arr
                              result = sum current

part2 :: [Integer] -> Integer
part2 sub = minimum sub + maximum sub

main = do
  contents <- readFile "Day09.txt"
  let entries = map (read :: String -> Integer) $ lines contents
  print $ part1 25 entries
  print $ part2 $ givenSumSub (part1 25 entries) entries
