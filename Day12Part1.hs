type Ferry = (Int, Int, Char)
type Action = (Char, Int)

parse :: String -> Action
parse action = (head action, read $ tail action)

interpret :: Ferry -> Action -> Ferry
interpret f@(_, _, dir) ('F', dist) = move f dir dist
interpret f ('L', deg) = turn f (-deg)
interpret f ('R', deg) = turn f deg
interpret f (dir, dist) = move f dir dist

move :: Ferry -> Char -> Int -> Ferry
move (x, y, dir) 'N' dist = (x, y+dist, dir)
move (x, y, dir) 'S' dist = (x, y-dist, dir)
move (x, y, dir) 'W' dist = (x-dist, y, dir)
move (x, y, dir) 'E' dist = (x+dist, y, dir)

turn :: Ferry -> Int -> Ferry
turn (x, y, dir) deg = (x, y, degreeToHeading newDirection)
  where newDirection = (360 + deg + (headingToDegree dir)) `mod` 360

headingToDegree :: Char -> Int
headingToDegree 'N' = 0
headingToDegree 'S' = 180
headingToDegree 'E' = 90
headingToDegree 'W' = 270

degreeToHeading :: Int -> Char
degreeToHeading 0 = 'N'
degreeToHeading 180 = 'S'
degreeToHeading 90 = 'E'
degreeToHeading 270 = 'W'

manhattan :: Ferry -> Int
manhattan (x,y,dir) = abs x + abs y

execute :: [Action] -> Ferry
execute = foldl interpret (0,0,'E')

part1 = do
  contents <- readFile "Day12.txt"
  let actions = map parse $ lines contents
  print $ manhattan $ execute actions
