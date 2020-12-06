import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.Set (fromList, size, intersection)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

answers :: ReadP [[String]]
answers = sepBy1 group (string "\n\n")
  where group = sepBy1 (many1 (satisfy isAlpha)) (string "\n")

main = do
  contents <- readFile "Day06.txt"
  let entries = parseMaybe answers contents  
  putStrLn $ show $ sum $ map size $ map fromList $ map concat $ fromJust entries
  putStrLn $ show $ sum $ map (size . foldr1 intersection . map fromList) $ fromJust entries
