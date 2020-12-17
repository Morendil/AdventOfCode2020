import qualified Data.Map as M
import Data.Maybe
import Data.List

type Point = (Int, Int, Int, Int)
type Cubes = M.Map Point Char

indexedGrid :: [String] -> Cubes
indexedGrid gridLines = M.fromList $ [((x,y,0,0), c) | (y, line) <- indexed gridLines, (x, c) <- indexed line]

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list

offsets :: [Point]
offsets = (map toPoint allOffsets) \\ [zero]
  where allOffsets = sequence [[-1,0,1],[-1,0,1],[-1,0,1],[-1,0,1]]
        zero = (0,0,0,0) :: Point
        toPoint [x,y,z,w] = (x,y,z,w)

add :: Point -> Point -> Point
add (x1,y1,z1,w1) (x2,y2,z2,w2) = (x1+x2,y1+y2,z1+z2,w1+w2)

neighbours :: Cubes -> Point -> [Char]
neighbours cubes point = mapMaybe ((flip M.lookup $ cubes) . (add point)) offsets

evolve :: Cubes -> Cubes
evolve cubes = M.fromList $ map evolveAt points
  where points = [(x,y,z,w) | x <- [xMin-1..xMax+1],
                            y <- [yMin-1..yMax+1],
                            z <- [zMin-1..zMax+1],
                            w <- [wMin-1..wMax+1]]
        evolveAt point = (point, evolveCell cubes point)
        xMin = minimum $ map (\(x,_,_,_) -> x) $ M.keys cubes
        xMax = maximum $ map (\(x,_,_,_) -> x) $ M.keys cubes
        yMin = minimum $ map (\(_,y,_,_) -> y) $ M.keys cubes        
        yMax = maximum $ map (\(_,y,_,_) -> y) $ M.keys cubes
        zMin = minimum $ map (\(_,_,z,_) -> z) $ M.keys cubes
        zMax = maximum $ map (\(_,_,z,_) -> z) $ M.keys cubes
        wMin = minimum $ map (\(_,_,_,w) -> w) $ M.keys cubes
        wMax = maximum $ map (\(_,_,_,w) -> w) $ M.keys cubes

evolveCell :: Cubes -> Point -> Char
evolveCell cubes pos = case prev of
    '#' -> if active == 2 || active == 3 then '#' else '.'
    '.' -> if active == 3  then '#' else '.'
    _ -> error "Uh?"
  where active = length $ filter (=='#') $ neighbours cubes pos
        prev = fromMaybe '.' $ M.lookup pos cubes

main = do
  contents <- readFile "sample.txt"
  let cubes = indexedGrid $ lines contents
  let evolved = last $ take 7 $ iterate evolve cubes
  print $ length $ filter (=='#') $ M.elems evolved
