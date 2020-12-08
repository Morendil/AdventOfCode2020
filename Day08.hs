import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Char

data OpCode = Nop | Acc | Jmp
  deriving (Eq, Show)
data Instr = Instr OpCode Int
  deriving (Eq, Show)
type State = (Int, Int, [Int])

main = do
  contents <- readFile "Day08.txt"
  let program = fromJust $ parseMaybe instructions contents
  putStrLn $ show $ execute program
  let mutated = mapMaybe (mutate program) [0..length program-1]
  putStrLn $ show $ filter (terminated program) $ map execute mutated

terminated :: [Instr] -> State -> Bool
terminated program state = pc == length program
  where (_, pc, _) = state

mutate :: [Instr] -> Int -> Maybe [Instr]
mutate program index = case program !! index of {
  Instr Acc arg -> Nothing;
  Instr Nop arg -> Just $ replace program index (Instr Jmp arg);
  Instr Jmp arg -> Just $ replace program index (Instr Nop arg)
}

replace :: [a] -> Int -> a -> [a]
replace list index item = take index list ++ [item] ++ drop (index+1) list

execute :: [Instr] -> State
execute program = visit program $ last $ takeWhile (not . done) $ trace
  where trace = iterate (visit program) (0, 0, [])
        done state = (loop state) || (out state)
        out (_, pc, history) = pc >= length program
        loop (_, pc, history) = pc `elem` history

visit :: [Instr] -> State -> State
visit program state@(_, pc, _) = step (program !! pc) state

step :: Instr -> State -> State
step (Instr Nop _) (acc, pc, history) = (acc, pc+1, pc:history)
step (Instr Acc arg) (acc, pc, history) = (acc+arg, pc+1, pc:history)
step (Instr Jmp arg) (acc, pc, history) = (acc, pc+arg, pc:history)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

instructions :: ReadP [Instr]
instructions = sepBy1 instruction (char '\n')

instruction :: ReadP Instr
instruction = choice $ map instr opcodes
  where opcodes = [(Nop, "nop"), (Acc, "acc"), (Jmp, "jmp")]

instr (cons, tag) = do
  string tag
  char ' '
  sign <- char '+' +++ char '-'
  arg <- many1 (satisfy isNumber)
  let signed = case sign of {'+' -> 1; '-' -> -1}
  return $ Instr cons ((read arg :: Int) * signed)

