import Data.Text (splitOn, pack, unpack)
import Data.Maybe
import Data.List
import Data.Ord

busId :: String -> Maybe Integer
busId "x" = Nothing
busId s = Just $ read s

-- https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a

pair :: Int -> Maybe Integer -> Maybe (Integer, Integer)
pair offset Nothing = Nothing
pair offset (Just id) = Just (toInteger (-offset), id)

main = do
  contents <- readFile "Day13.txt"
  let spec = lines contents
  let earliest = (read $ head spec) :: Integer
  let ids = splitOn (pack ",") (pack $ last spec)
  let nextTime x = x - (earliest `mod` x)
  let time x = if nextTime x == 0 then 0 else nextTime x
  let nextBus = minimumBy (comparing time) $ mapMaybe (busId . unpack) ids
  -- part 1
  print $ (nextTime nextBus) * nextBus
  -- part 2
  print $ crt $ catMaybes $ zipWith pair [0..length ids-1] (map (busId . unpack) ids)
