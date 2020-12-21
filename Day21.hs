import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Map as M

type Dish = ([String], [String])
type Guess = M.Map String [String]

main = do
  contents <- readFile "Day21.txt"
  let entries = fromJust $ parseMaybe dishes contents
      ingredients = nub $ concatMap fst entries
      allergens = nub $ concatMap snd entries
      restricted = foldr (restrict entries) M.empty allergens
      solved = converge eliminate restricted
      dangerous = concat $ M.elems solved
      clean = ingredients \\ dangerous
  -- part1 
  print $ length $ concatMap (intersect clean . fst) entries
  -- part2 (sort order is taken care of by M.elems)
  print $ intercalate "," dangerous

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

eliminate :: Guess -> Guess
eliminate prev = foldr remove prev single
  where single = M.keys $ M.filter ((==1).length) $ prev
        assigned allergen = head $ fromJust $ M.lookup allergen prev
        remove allergen prev = M.map (crossOut ingr) prev
          where ingr = assigned allergen
        crossOut ingr list = if length list > 1 then list \\ [ingr] else list

restrict :: [Dish] -> String -> Guess -> Guess
restrict entries allergen prev = foldr (cross allergen) prev entries

cross :: String -> Dish -> Guess -> Guess
cross allergen (dI,dA) prev = if elem allergen dA then upd else prev
  where upd = M.insert allergen (intersect old dI) prev
        old = fromMaybe dI $ M.lookup allergen prev

dishes :: ReadP [Dish]
dishes = sepBy1 dish (string "\n")

dish :: ReadP Dish
dish = (,) <$> ingredients <*> between p1 p2 allergens
  where word = many1 (satisfy isAlpha)
        ingredients = sepBy1 word (string " ")
        allergens = sepBy1 word (string ", ")
        p1 = string " (contains "
        p2 = string ")"

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
