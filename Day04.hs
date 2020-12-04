import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Text.Read (readMaybe)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

field :: ReadP (String, String)
field = do
  key <- many1 (satisfy isAlpha)
  string ":"
  val <- many1 (satisfy (not . isSpace))
  return (key, val)

fields :: ReadP [(String, String)]
fields = sepBy1 field (satisfy isSpace)

passports :: ReadP [[(String, String)]]
passports = sepBy1 fields newlines
  where newlines = string "\n\n"
        anything = munch (const True)

required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validPart1 :: [(String, String)] -> Bool
validPart1 passport = all id $ map (hasField passport) required
  where hasField passport key = isJust $ lookup key passport

rules = [
  ("byr",validByr),
  ("iyr",validIyr),
  ("eyr",validEyr),
  ("hgt",validHgt),
  ("hcl",validHcl),
  ("ecl",validEcl),
  ("pid",validPid)]

validHcl ('#':rest) = (length rest == 6) && all isHexDigit rest
validHcl _ = False

validPid value = (length value == 9) && isJust numeric
  where numeric = (readMaybe value) :: Maybe Int

validEcl = isJust . parseMaybe (choice choices)
  where choices = map string ["amb","blu","brn","gry","grn","hzl","oth"]

validByr = years 1920 2019
validIyr = years 2010 2020
validEyr = years 2020 2030

data Height = In Int | Cm Int
height :: ReadP Height
height = choice [aHeight Cm "cm", aHeight In "in"]
aHeight which tag = do
  value <- many1 (satisfy isNumber)
  string tag
  return $ which (read value)

validHgt value = maybe False good $ parseMaybe height value
  where good (Cm h) = h >= 150 && h <= 193
        good (In h) = h >= 59 && h <= 76

years minyr maxyr value = (length value == 4) && maybe False good yr
  where yr = readMaybe value
        good n = n >= minyr && n <= maxyr

validField :: [(String, String)] -> String -> Bool
validField passport key = maybe False validate value
  where value = lookup key passport
        validate = rule key
        rule key = fromJust $ lookup key rules

valid :: [(String, String)] -> Bool
valid passport = all id $ map (validField passport) required

main = do
  contents <- readFile "Day04.txt"
  let entries = parseMaybe passports contents
  putStrLn $ show $ length $ filter valid $ fromJust entries
