import qualified Data.Map as M
import Data.Maybe
import Data.List

main = do
  print $ part1 [3,1,2]

type Turns = (Int, Int)
type State = (M.Map Int Turns, Int, Int)

part1 :: [Int] -> Int
part1 numbers = last $ take (2020-3) $ unfoldr (Just . step) (initial numbers)

initial :: [Int] -> State
initial numbers = (M.fromList seed, last numbers, length numbers + 1)
  where seed = (zip numbers (zip (repeat 0) [1..length numbers]))

step :: State -> (Int, State)
step (prev, lastN, turn) = case M.lookup lastN prev of {
    Just (0,t1) -> (0, (M.insert 0 (z2,turn) prev, 0, turn+1));
    Just (t1,t2) -> (t2-t1, (M.alter (upd turn) (t2-t1) prev, t2-t1, turn+1));
  }
  where (z1,z2) = fromMaybe (0,0) $ M.lookup 0 prev
        upd t0 = Just . maybe (0,t0) (\(t1,t2)->(t2,t0))
