import qualified Data.Map as M
import Data.Maybe

type Point = (Int, Int)
type Seats = M.Map Point Char

indexedGrid :: [String] -> Seats
indexedGrid gridLines = M.fromList $ [((x,y), c) | (y, line) <- indexed gridLines, (x, c) <- indexed line]

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list

offsets :: [Point]
offsets = [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

neighbours :: Seats -> Point -> [Char]
neighbours seats point = mapMaybe ((flip M.lookup $ seats) . (add point)) offsets

evolve :: Seats -> Seats
evolve seats = M.mapWithKey (evolveCell seats) seats

evolveCell :: Seats -> Point -> Char -> Char
evolveCell seats pos prev = case prev of
    '.' -> '.'
    'L' -> if occupied == 0 then '#' else 'L'
    '#' -> if occupied >= 4  then 'L' else '#'
    _ -> error "Uh?"
  where occupied = length $ filter (=='#') $ neighbours seats pos

display :: Seats -> [String]
display seats = [ [ fromJust $ M.lookup (x,y) seats | x <- [0..xMax]] | y <- [0..yMax]]
  where xMax = maximum $ map fst $ M.keys seats
        yMax = maximum $ map snd $ M.keys seats

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
  contents <- readFile "Day11.txt"
  let seats = indexedGrid $ lines contents
  -- print seats
  -- putStrLn $ unlines $ display $ evolve seats
  -- putStrLn $ unlines $ display $ evolve $ evolve seats
  let stable = converge evolve seats
  putStrLn $ show $ length $ M.filter (=='#') stable
