import Data.List
import Data.Maybe

main = do
  let (card, door) = (1717001, 523731)
  -- let (card, door) = (5764801, 17807724)
      cardLoop = loopSize card
      doorLoop = loopSize door
  print $ cardLoop
  print $ doorLoop
  print $ modPow card doorLoop modulus
  print $ modPow door cardLoop modulus

modulus :: Integer
modulus = 20201227

loopSize :: Integer -> Integer
loopSize remainder = giantStep - babyStep
  where s = intSqrt modulus
        giant g = (modPow 7 g modulus, g)
        baby b = ((remainder*(modPow 7 b modulus)) `mod` modulus, b)      
        giants = [giant (g*s) | g <- [1..s]]
        babies = [baby b | b <- [1..s-1]]
        common = head $ intersect (map fst giants) (map fst babies)
        giantStep = fromJust $ lookup common giants
        babyStep = fromJust $ lookup common babies

intSqrt :: Integer -> Integer
intSqrt = ceiling . sqrt . fromInteger

modPow :: Integer -> Integer -> Integer -> Integer
modPow b e m | e >= 0 = powm b e m 1
modPow b e m | e < 0 = modInv (powm b (-e) m 1) m

powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

modInv :: Integer -> Integer -> Integer
modInv a m = modPow a (m-2) m
