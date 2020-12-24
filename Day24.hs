import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Point = (Int,Int,Int)
type Pattern = M.Map Point Bool

add :: Point -> Point -> Point
add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

offsets :: [(String, Point)]
offsets = [ ("e",(1,-1,0)),
            ("se",(0,-1,1)),
            ("sw",(-1,0,1)),
            ("w",(-1,1,0)),
            ("nw",(0,1,-1)),
            ("ne",(1,0,-1))]

dirs :: [String]
dirs = ["e", "se", "sw", "w", "nw", "ne"]

apply :: Point -> [String] -> Point
apply start = foldl step start
  where step pt = add pt . offset
        offset dir = fromJust $ lookup dir offsets

flipTile :: Pattern -> Point -> Pattern
flipTile pattern pt = M.alter flipOrInsert pt pattern
  where flipOrInsert Nothing = Just True
        flipOrInsert (Just previous) = Just $ not previous

neighbours :: Point -> [Point]
neighbours pt = map (add pt . snd) offsets

pointsToConsider :: Pattern -> [Point]
pointsToConsider pattern = nub $ concat allPoints ++ blacks
  where allPoints = map neighbours blacks
        blacks = M.keys $ M.filter id pattern

evolveOne :: Pattern -> Pattern -> Point -> Pattern
evolveOne prev next pt = M.insert pt newValue next
  where current = fromMaybe False $ M.lookup pt prev
        nearby = map (flip M.lookup $ prev) (neighbours pt)
        count = length $ filter id $ catMaybes nearby
        newValue = case current of {
    True -> if count == 0 || count > 2 then False else True;
    False -> if count == 2 then True else False}

evolve :: Pattern -> Pattern
evolve prev = foldl (evolveOne prev) prev relevant
  where relevant = pointsToConsider prev        

main = do
  contents <- readFile "sample.txt"
  let entries = fromJust $ parseMaybe instructions contents
  let tiles = map (apply (0,0,0)) entries
  let result = foldl flipTile M.empty tiles
      final = last $ take 101 $ iterate evolve result
  -- part1
  print $ M.size $ M.filter id result
  -- part2
  print $ M.size $ M.filter id $ final
  

direction :: ReadP String
direction = choice $ map string dirs

directions :: ReadP [String]
directions = many1 direction

instructions :: ReadP [[String]]
instructions = sepBy1 directions (string "\n")

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result