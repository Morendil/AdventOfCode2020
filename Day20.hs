import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.List
import Data.Function
import qualified Data.Map as M
import Debug.Trace

type Edge = String
type Tile = [String]
type Fits = (Int, ([[Int]], Tile))
type Arrangement = [Fits]
type Labeled = (Int, Tile)
type Tiles = [Labeled]
type Placed = M.Map Int ([[Int]], Tile)
  
placeCol :: Placed -> [Fits] -> Fits -> (Placed, [Fits])
placeCol placed col (_, ([_,_,_,[]], _)) = (placed, col)
placeCol placed col (tId, thisPiece) = placeCol placed' col' (nxt,next)
  where ([_,_,_,[nxt]], constraining) = thisPiece
        (original, grid) = fromJust $ M.lookup nxt placed
        neighbours = concat original
        get nId = snd $ fromJust $ M.lookup nId placed
        goodWith fn = filter (fits (fn newGrid) . get) neighbours
        newOrder = map goodWith edgeOps
        constraint = last constraining
        candidates = orientations grid
        newGrid = head $ filter (\g -> constraint == head g) candidates
        next = (newOrder, newGrid)
        col' = col ++ [(nxt,next)]
        placed' = M.insert nxt next placed

placeRow :: Placed -> [Fits] -> Fits -> (Placed, [Fits])
placeRow placed row (_, ([_,_,[],_], _)) = (placed, row)
placeRow placed row (tId, thisPiece) = placeRow placed' row' (nxt,next)
  where ([_,_,[nxt],_], constraining) = thisPiece
        (original, grid) = fromJust $ M.lookup nxt placed
        neighbours = concat original
        get nId = snd $ fromJust $ M.lookup nId placed
        goodWith fn = filter (fits (fn newGrid) . get) neighbours
        newOrder = map goodWith edgeOps
        constraint = map last constraining
        candidates = orientations grid
        newGrid = head $ filter (\g -> constraint ==  map head g) candidates
        next = (newOrder, newGrid)
        row' = row ++ [(nxt,next)]
        placed' = M.insert nxt next placed

main = do
  contents <- readFile "Day20.txt"
  let tileList = fromJust $ parseMaybe tiles contents
      arranged = map (findNeighbours tileList) tileList
      cornerTiles = filter corner arranged    
      placed = M.fromList arranged  
  -- part1
  print $ product $ map fst cornerTiles
  -- part2
  let anchor = head $ filter topLeft arranged
      (placed', cols) = placeCol placed [anchor] anchor
      makeRow (prev, list) piece = let (p,r) = placeRow prev [piece] piece in (p,list++[r])
      arrangement = snd $ foldl makeRow (placed',[]) cols
      expanded = map (map (trimEdges.snd.snd)) arrangement
      unpuzzled = concatMap (foldr1 (zipWith (++))) expanded
      seaTiles = length $ filter (=='#') $ concat unpuzzled
      monsterCount = maximum $ map monsters $ orientations unpuzzled
      monTiles = 15 * monsterCount
      ori3 = orientations unpuzzled !! 3
  print $ length ori3
  print $ monsters ori3
  print seaTiles
  print $ seaTiles - monTiles
  -- print $ lookup 1019 arranged
  -- print $ lookup 1283 arranged
  -- print $ lookup 3727 arranged
  -- print $ placeCol placed [anchor] $ anchor

windows l = map (\n -> take 3 $ drop n l) [0..(length l-3)]
asLists = map (elemIndices '#')

monsters :: [String] -> Int
monsters grid = sum $ map monstersIn $ windows $ asLists grid

orientations :: [[a]] -> [[[a]]]
orientations = take 8 . flip (scanl (&)) ops
  where ops = concat $ replicate 4 [transpose, map reverse]

monstersIn :: [[Int]] -> Int
monstersIn l = length $ filter id $ map (`foundAt` l) [0..(96-19)]
  where foundAt n = and . zipWith isSubsequenceOf (map (map (+n)) monster)

monster :: [[Int]]
monster = map (elemIndices '#') ["                  #","#    ##    ##    ###"," #  #  #  #  #  #"]

trimEdges :: [String] -> [String]
trimEdges = trim . map trim
  where trim l = take (length l -2) $ tail l

topLeft :: Fits -> Bool
topLeft (_, (fits, _)) = map length fits == [0,0,1,1] 

corner :: Fits -> Bool
corner (_, (fits, _)) = length (filter (not.null) fits) == 2

edgeOps = [head, map head, map last, last]

edges :: Tile -> [Edge]
edges tile = map ($ tile) edgeOps

fits :: Edge -> Tile -> Bool
fits edge tile = edge `elem` otherEdges
  where otherEdges = edges tile ++ map reverse (edges tile)

findNeighbours :: Tiles -> Labeled -> Fits
findNeighbours tiles labeled = (tileId, (findFits allOthers tile, tile))
  where (tileId, tile) = labeled
        allOthers = tiles \\ [labeled]

findFits :: Tiles -> Tile -> [[Int]]
findFits tiles tile = map (findEdgeFits tiles) (edges tile)

findEdgeFits :: Tiles -> Edge -> [Int]
findEdgeFits tiles edge = map getId matching
  where matching = filter (\(tId, img) -> fits edge img) tiles
        getId (tId,_) = tId

tiles :: ReadP Tiles
tiles = sepBy1 tile (string "\n\n")

tile :: ReadP Labeled
tile = (,) <$> header <*> sepBy1 line (string "\n")
  where line = many1 (satisfy (`elem` "#."))

header :: ReadP Int
header = string "Tile " >> number <* string ":\n"

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
