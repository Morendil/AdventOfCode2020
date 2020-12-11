import qualified Data.Map as M
import Data.Maybe

type Point = (Int, Int)
type Seats = M.Map Point Char

indexedGrid :: [String] -> Seats
indexedGrid gridLines = M.fromList $ [((x,y), c) | (y, line) <- indexed gridLines, (x, c) <- indexed line, c /= '.']

guard :: Seats -> Seats
guard seats = M.fromList $ M.toList seats ++ [(g,'.')|g<-guards]
  where guards = concat caps ++ concat borders
        caps = [[(x,-1),(x,yMax+1)]|x<-[-1..xMax+1]]
        borders = [[(-1,y),(xMax+1,y)]|y<-[0..yMax]]
        xMax = maximum $ map fst $ M.keys seats
        yMax = maximum $ map snd $ M.keys seats

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list

offsets :: [Point]
offsets = [(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1)]

add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

neighbours :: Seats -> Point -> [Char]
neighbours seats point = map (visible seats point) offsets

visible :: Seats -> Point -> Point -> Char
visible seats from offset = head $ mapMaybe (flip M.lookup seats) $ tail $ iterate (add offset) from

evolve :: Seats -> Seats
evolve seats = M.mapWithKey (evolveCell seats) seats

evolveCell :: Seats -> Point -> Char -> Char
evolveCell seats pos prev = case prev of
    '.' -> '.'
    'L' -> if occupied == 0 then '#' else 'L'
    '#' -> if occupied >= 5  then 'L' else '#'
    _ -> error "Uh?"
  where occupied = length $ filter (=='#') $ neighbours seats pos

display :: Seats -> [String]
display seats = [ [ fromMaybe '.' $ M.lookup (x,y) seats | x <- [0..xMax]] | y <- [0..yMax]]
  where xMax = maximum $ map fst $ M.keys seats
        yMax = maximum $ map snd $ M.keys seats

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
  contents <- readFile "Day11.txt"
  let seats = guard $ indexedGrid $ lines contents
  -- putStrLn $ unlines $ display $ evolve $ evolve seats
  let stable = converge evolve seats
  print $ length $ M.filter (=='#') stable
