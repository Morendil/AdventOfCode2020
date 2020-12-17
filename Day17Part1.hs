import qualified Data.Map as M
import Data.Maybe
import Data.List

type Point = (Int, Int, Int)
type Cubes = M.Map Point Char

indexedGrid :: [String] -> Cubes
indexedGrid gridLines = M.fromList $ [((x,y,0), c) | (y, line) <- indexed gridLines, (x, c) <- indexed line]

indexed :: [a] -> [(Int, a)]
indexed list = zip [0..(length list)] list

offsets :: [Point]
offsets = (map toPoint allOffsets) \\ [zero]
  where allOffsets = sequence [[-1,0,1],[-1,0,1],[-1,0,1]]
        zero = (0,0,0) :: Point
        toPoint [x,y,z] = (x,y,z)

add :: Point -> Point -> Point
add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

neighbours :: Cubes -> Point -> [Char]
neighbours cubes point = mapMaybe ((flip M.lookup $ cubes) . (add point)) offsets

evolve :: Cubes -> Cubes
evolve cubes = M.fromList $ map evolveAt points
  where points = [(x,y,z) | x <- [xMin-1..xMax+1],
                            y <- [yMin-1..yMax+1],
                            z <- [zMin-1..zMax+1]]
        evolveAt point = (point, evolveCell cubes point)
        xMin = minimum $ map (\(x,_,_) -> x) $ M.keys cubes
        xMax = maximum $ map (\(x,_,_) -> x) $ M.keys cubes
        yMin = minimum $ map (\(_,y,_) -> y) $ M.keys cubes        
        yMax = maximum $ map (\(_,y,_) -> y) $ M.keys cubes
        zMin = minimum $ map (\(_,_,z) -> z) $ M.keys cubes
        zMax = maximum $ map (\(_,_,z) -> z) $ M.keys cubes

evolveCell :: Cubes -> Point -> Char
evolveCell cubes pos = case prev of
    '#' -> if active == 2 || active == 3 then '#' else '.'
    '.' -> if active == 3  then '#' else '.'
    _ -> error "Uh?"
  where active = length $ filter (=='#') $ neighbours cubes pos
        prev = fromMaybe '.' $ M.lookup pos cubes

displaySlice :: Cubes -> Int -> String
displaySlice cubes slice = "z=" ++ show slice ++ "\n" ++ unlines [[ fromMaybe '.' $ M.lookup (x,y,slice) cubes | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where xMin = minimum $ map (\(x,_,_) -> x) $ M.keys cubes
        xMax = maximum $ map (\(x,_,_) -> x) $ M.keys cubes
        yMin = minimum $ map (\(_,y,_) -> y) $ M.keys cubes        
        yMax = maximum $ map (\(_,y,_) -> y) $ M.keys cubes

display :: Cubes -> String
display cubes = unlines $ map (displaySlice cubes) [zMin..zMax]
  where zMin = minimum $ map (\(_,_,z) -> z) $ M.keys cubes
        zMax = maximum $ map (\(_,_,z) -> z) $ M.keys cubes

main = do
  contents <- readFile "Day17.txt"
  let cubes = indexedGrid $ lines contents
  -- part 1
  let evolved = last $ take 7 $ iterate evolve cubes
  print $ length $ filter (=='#') $ M.elems evolved
