import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (mapMaybe)

main = do
  contents <- readFile "Day18.txt"
  let results = mapMaybe (parseMaybe part2) $ lines contents
  print results
  print $ sum results

plus = string " + " >> return (+)
times = string " * " >> return (*)

part1 = chainl1 (number +++ paren part1) (plus +++ times)
part2 = chainl1 (chainl1 (number +++ paren part2) plus) times

paren p = do
  string "("
  contents <- p
  string ")"
  return contents

number :: ReadP Integer
number = do
  lit <- many1 (satisfy isNumber)
  return $ read lit

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
