type Pos = (Int, Int)
type Ferry = (Pos, Pos)
type Action = (Char, Int)

parse :: String -> Action
parse action = (head action, read $ tail action)

interpret :: Ferry -> Action -> Ferry
interpret f@(pos, wp) ('F', dist) = move f dist
interpret (pos,(wx,wy)) ('R', 90) = (pos,(wy,-wx))
interpret (pos,(wx,wy)) ('R', 180) = (pos,(-wx,-wy))
interpret (pos,(wx,wy)) ('R', 270) = (pos,(-wy,wx))
interpret f ('L', 90) = interpret f ('R', 270)
interpret f ('L', 180) = interpret f ('R', 180)
interpret f ('L', 270) = interpret f ('R', 90)
interpret f (dir, dist) = retarget f dir dist

move :: Ferry -> Int -> Ferry
move ((x,y),(dx,dy)) dist = ((x+(dist*dx),y+(dist*dy)),(dx,dy))

retarget :: Ferry -> Char -> Int -> Ferry
retarget (p, (x,y)) 'N' dist = (p,(x, y+dist))
retarget (p, (x,y)) 'S' dist = (p,(x, y-dist))
retarget (p, (x,y)) 'W' dist = (p,(x-dist, y))
retarget (p, (x,y)) 'E' dist = (p,(x+dist, y))

turn :: Ferry -> Int -> Ferry
turn f deg = f

manhattan :: Ferry -> Int
manhattan ((x,y),_) = abs x + abs y

execute :: [Action] -> Ferry
execute = foldl interpret ((0,0),(10,1))

main = do
  contents <- readFile "Day12.txt"
  let actions = map parse $ lines contents
  print $ manhattan $ execute actions
