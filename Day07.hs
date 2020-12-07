import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.Graph
import Data.List

type Bag = (String, [(Int, String)])

main = do
  contents <- readFile "Day07.txt"
  let entries = fromJust $ parseMaybe bags contents
  let edgeList = map edge entries
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList
  let pathToSg (desc,_) = path graph (fromJust $ vertexFromKey desc) (fromJust $ vertexFromKey "shinygold")
  putStrLn $ show $ (length $ filter pathToSg entries) - 1
  putStrLn $ show $ bagsContained entries (findBag entries "shinygold")

findBag :: [Bag] -> String -> Bag
findBag bags name = head $ filter (\(desc,_) -> desc == name) bags

bagsContained :: [Bag] -> Bag -> Int
bagsContained _ (_, []) = 0
bagsContained bags (desc, entries) = contents + innerContents
  where contents = sum $ map fst entries
        innerContents = sum $ map bagsIn entries
        bagsIn (weight, name) = weight * bagsContained bags (findBag bags name)

color :: ((), String, [String]) -> String
color (_, desc, inner) = desc

edge :: Bag -> ((), String, [String])
edge (desc, inner) = ((), desc, map snd inner)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

description :: ReadP String
description = do
  adjective <- many1 (satisfy isAlpha)
  char ' '
  color <- many1 (satisfy isAlpha)
  return $ adjective ++ color

inner :: ReadP (Int, String)
inner = do
  count <- many1 (satisfy isNumber)
  char ' '
  desc <- description
  string " bag"
  optional (char 's')
  return (read count, desc)

bag :: ReadP Bag
bag = do
  desc <- description
  inside <- choice [empty, nonEmpty]
  return (desc, inside)
  where empty = do
          string " bags contain no other bags."
          return []    
        nonEmpty = do
          string " bags contain "
          list <- sepBy1 inner (string ", ")
          string "."
          return list

bags :: ReadP [Bag]
bags = sepBy1 bag (char '\n')
