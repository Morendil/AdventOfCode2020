import Data.List
import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (mapMaybe, fromJust, isJust)
import qualified Data.Map as M

type Rule = (Int, Prod)
data Prod = Lit Char | Alt [[Int]] | Seq [Int]
  deriving Show
type Rules = M.Map Int Prod

makeParser :: Rules -> ReadP ()
makeParser rules = do
  result <- mkParser rules $ fromJust $ M.lookup 0 rules
  eof
  return result

mkParser :: Rules -> Prod -> ReadP ()
mkParser rules (Lit c) = char c >> return ()
mkParser rules (Alt as) = choice $ map (mkParser rules . Seq) as
mkParser rules (Seq is) = do
  -- insert 8 & 11 rules for part2, delete for part1
  let deref 8 = do {many1 (deref 42); return ()}
      deref 11 = do {deref 42; optional (deref 11); deref 31; return ()}
      deref i = mkParser rules $ fromJust $ M.lookup i rules
  coll <- sequence $ map deref is
  return $ last coll

rule :: ReadP Rule
rule = do
  index <- number
  string ": "
  prod <- choice [literal, list, fork]
  return (index, prod)

literal :: ReadP Prod
literal = do
  it <- between (char '"') (char '"') get
  return $ Lit it

fork :: ReadP Prod
fork = do
  values <- sepBy1 (sepBy1 number (string " ")) (string " | ")
  return $ Alt values

list :: ReadP Prod
list = do
  values <- sepBy1 number (string " ")
  return $ Seq values

main = do
  contents <- readFile "Day19.txt"
  let entries = filter (not.null) $ lines contents
  let (messages, rules) = partition (flip elem "ab" . head) entries
  let ruleMap = M.fromList $ mapMaybe (parseMaybe rule) rules
  let parser = makeParser ruleMap
  putStrLn $ unlines $ filter (isJust . parseMaybe parser) messages
  print $ length $ (filter id) $ map (isJust . parseMaybe parser) messages

number :: ReadP Int
number = do
  lit <- many1 (satisfy isNumber)
  return $ (read lit)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
