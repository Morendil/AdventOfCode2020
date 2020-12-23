import Data.List
import Data.Maybe
import qualified Data.IntMap as M
import Debug.Trace

type State = (Int, M.IntMap Int)

shuffle :: Int -> State -> State
shuffle maxCup (cur, cups) = (follow, altered)
  where taken = tail $ take 4 $ iterate (next cups) cur
        dest = pick maxCup (cur-1) taken
        follow = next cups (last taken)
        after = next cups dest
        altered = M.insert cur follow $
                  M.insert dest (head taken) $
                  M.insert (last taken) (next cups dest) $
                    cups

next :: M.IntMap Int -> Int -> Int
next m n = fromMaybe (succ n) $ M.lookup n m

pick :: Int -> Int -> [Int] -> Int
pick maxCup target taken | target `elem` taken = pick maxCup (target-1) taken
pick maxCup target taken | target <= 0 = pick maxCup maxCup taken
pick maxCup target taken = target

build :: Int -> [Int] -> State
build maxCup list = (head list, cups)
  where start = M.fromList $ zip list (tail list)
        cups = M.insert lastOne (head list) start
        lastOne = if maxCup `elem` list then last list else maxCup

afterOne :: Int -> State -> [Int]
afterOne n s = tail $ take (n+1) $ iterate (next cups) 1
  where (_,cups) = s

part1 = do
  let maxCup = 9
      initial = build maxCup [3,8,9,1,2,5,4,6,7]
      states = iterate (shuffle maxCup) initial
      go 0 state = state
      go n state = go (n-1) $ shuffle maxCup state
  print $ afterOne 8 $ go 100 initial

part2 = do
  let maxCup = 1000000
      initial = build maxCup [3,8,9,1,2,5,4,6,7]
      states = iterate (shuffle maxCup) initial
      go 0 state = state
      go n state = go (n-1) $ shuffle maxCup state
  print $ afterOne 2 $ go 100000000 initial

main = do
    part2