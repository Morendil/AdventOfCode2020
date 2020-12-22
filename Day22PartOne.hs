import Data.Text (splitOn, pack, unpack)

play :: [[Int]] -> [[Int]]
play ([[],p2]) = [[],p2]
play ([p1,[]]) = [p1,[]]
play ([c1:r1,c2:r2]) = if c1 > c2 then p1win else p2win
  where p1win = [r1++[c1,c2],r2]
        p2win = [r1,r2++[c2,c1]]

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

score :: [[Int]] -> Int
score = sum . mark . head . filter (not.null)
  where mark cs = zipWith (*) cs (ranks cs)
        ranks cs = reverse [1..length cs]

main = do
  contents <- readFile "Day22.txt"
  let raw = splitOn (pack "\n\n") (pack $ contents)
      split = map (tail . splitOn (pack "\n")) raw
      decks :: [[Int]]
      decks = map (map (read . unpack)) split
  print $ score $ converge play decks

