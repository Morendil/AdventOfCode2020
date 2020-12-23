import Control.Monad.ST
import Data.Array.ST
import Data.List

shuffle :: Int -> (Int, STUArray s Int Int) -> ST s (Int, STUArray s Int Int)
shuffle maxCup (current, array) = do
  one <- readArray array current
  two <- readArray array one
  three <- readArray array two
  let taken = [one, two, three]
      dest = pick maxCup (current-1) taken
  follow <- readArray array three
  after <- readArray array dest
  writeArray array current follow
  writeArray array dest one
  writeArray array three after
  return (follow, array)

pick :: Int -> Int -> [Int] -> Int
pick maxCup target taken | target `elem` taken = pick maxCup (target-1) taken
pick maxCup target taken | target <= 0 = pick maxCup maxCup taken
pick maxCup target taken = target

runShuffle 0 _ state = return state
runShuffle n maxCup state = do
  s1 <- shuffle maxCup state
  runShuffle (n-1) maxCup s1
  
doShuffles = do
  let maxCup = 1000000
      list = [10,8,9,5,7,1,3,4,6]++[11..maxCup]++[2]
  arr <- newListArray (1, maxCup) list :: ST s (STUArray s Int Int)
  runShuffle 10000000 maxCup (2, arr)
  a <- readArray arr 1
  b <- readArray arr a
  return $ (a,b)

main = print $ runST doShuffles