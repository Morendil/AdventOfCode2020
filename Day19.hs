import Data.List
import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (mapMaybe, fromJust, isJust)
import qualified Data.Map as M
import Control.Monad (void)

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
mkParser rules (Lit c) = void (char c)
mkParser rules (Alt as) = choice $ map (mkParser rules . Seq) as
mkParser rules (Seq is) = mapM_ deref is
  where deref i = mkParser rules $ fromJust $ M.lookup i rules

rule :: ReadP Rule
rule = (,) <$> number <*> (string ": " *> choice [literal, list, fork])
  -- all ReadP :: Prod
  where literal = Lit <$> between (char '"') (char '"') get
        fork = Alt <$> sepBy1 (sepBy1 number (string " ")) (string " | ")
        list = Seq <$> sepBy1 number (string " ")

main = do
  contents <- readFile "Day19.txt"
  let entries = filter (not.null) $ lines contents
  let (messages, rules) = partition (flip elem "ab" . head) entries
  let ruleMap = M.fromList $ mapMaybe (parseMaybe rule) rules
  let part1 = makeParser ruleMap
  print $ length $ filter id $ map (isJust . parseMaybe part1) messages
  let ruleMap' = M.insert 8 (Alt [[42],[42,8]]) $ M.insert 11 (Alt [[42,31],[42,11,31]]) ruleMap
  let part2 = makeParser ruleMap'
  print $ length $ filter id $ map (isJust . parseMaybe part2) messages

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
