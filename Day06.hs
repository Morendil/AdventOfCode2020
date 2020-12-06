import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.Set (fromList, size, intersection, union)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

answers :: ReadP [[String]]
answers = sepBy1 group (string "\n\n")
  where group = sepBy1 (many1 (satisfy isAlpha)) (string "\n")

solution fn = sum . map (size . foldr1 fn . map fromList) . fromJust

main = do
  contents <- readFile "Day06.txt"
  let entries = parseMaybe answers contents
  -- part1
  putStrLn $ show $ solution union entries
  -- part2
  putStrLn $ show $ solution intersection entries
