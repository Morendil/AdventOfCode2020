import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe (fromJust)
import Data.Ix (inRange)
import Data.List (transpose, (\\), isPrefixOf)

type Range = (Int,Int)
type Spec = (String, [Range])
type Ticket = [Int]

part1 :: [Spec] -> [Ticket] -> Int
part1 specs = sum . concatMap (invalidValues specs)

invalidValues :: [Spec] -> Ticket -> Ticket
invalidValues specs = filter invalid
  where invalid value = not $ any id $ map (`validBy` value) specs

canBeValid :: [Spec] -> Ticket -> Bool
canBeValid specs = all id . map canBeValidValue
  where canBeValidValue value = any id $ map (`validBy` value) specs

doesValidate :: [Int] -> Spec -> Bool
doesValidate values spec = all id $ map (validBy spec) values

possibilities :: [Spec] -> [Ticket] -> [[Spec]]
possibilities specs tickets = map candidates fields
  where fields = transpose tickets
        candidates values = filter (doesValidate values) specs

validBy :: Spec -> Int -> Bool
validBy (_, ranges) value = any (`inRange` value) ranges

solve :: [Spec] -> [Ticket] -> [Spec]
solve specs nearby = map head $ converge fix $ possibilities specs $ filter (canBeValid specs) nearby

fix :: [[Spec]] -> [[Spec]]
fix possibles = map winnow possibles
  where fixed = filter ((==)1.length) possibles
        winnow candidates = foldl eliminate candidates fixed
        eliminate [x] candidate = [x]
        eliminate list candidate = list \\ candidate

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
  contents <- readFile "Day16.txt"
  let (specs, mine, nearby) = fromJust $ parseMaybe puzzle contents
  print $ part1 specs nearby
  -- part 2
  let solved = solve specs nearby
  let values = zipWith (\(name,_) value -> (name,value)) solved mine
  let relevant (name,_) = isPrefixOf "departure" name
  print $ product $ map snd $ filter relevant values

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

puzzle :: ReadP ([Spec], Ticket, [Ticket])
puzzle = do
  specs <- sepBy1 spec (string "\n")
  string "\n\nyour ticket:\n"
  mine <- ticket
  string "\n\nnearby tickets:\n"
  nearby <- sepBy1 ticket (string "\n")
  return $ (specs, mine, nearby)
  
ticket :: ReadP Ticket
ticket = do
  values <- sepBy1 (many1 (satisfy isNumber)) (char ',')
  return $ map read values

range :: ReadP (Int, Int)
range = do
  lo <- many1 (satisfy isNumber)
  char '-'
  hi <- many1 (satisfy isNumber)
  return $ (read lo, read hi)

spec :: ReadP Spec
spec = do
  name <- many1 $ satisfy (/= ':')
  string ": "
  ranges <- sepBy1 range (string " or ")
  return $ (name, ranges)