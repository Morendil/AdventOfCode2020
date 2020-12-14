import Text.ParserCombinators.ReadP
import Data.Maybe
import Data.Char
import Data.Bits
import qualified Data.Map as M

type State = (Mask, M.Map Int Int)
type Mask = String
type Access = (Int, Int)
data Instruction = SetMask Mask | Poke Access

answer :: State -> Int
answer (_, mem) = sum $ M.elems mem

execute :: [Instruction] -> State
execute = foldl apply ("", M.empty)

apply :: State -> Instruction -> State
apply (_, mem) (SetMask mask') = (mask', mem)
apply (mask, mem) (Poke (addr, val)) = (mask, newMem)
  where newMem = foldl insert mem (floatMask mask)
        addr' = doMask1 mask addr
        insert oneMem oneMask = M.insert (doMask oneMask addr') val oneMem

floatMask :: String -> [String]
floatMask = sequence . map float
  where float 'X' = ['0','1']
        float c = ['X']

doMask1 :: String -> Int -> Int
doMask1 mask val = val .|. ones
  where ones = decode (=='1') mask
        decode fn = unbinary . map (fromEnum . fn)

doMask :: String -> Int -> Int
doMask mask val = (val .&. zeroes) .|. ones
  where ones = decode (=='1') mask
        zeroes = decode (/='0') mask
        decode fn = unbinary . map (fromEnum . fn)

main = do
  contents <- readFile "Day14.txt"
  let program = fromJust $ parseMaybe (sepBy1 instruction (string "\n")) contents
  print $ answer $ execute program

unbinary :: [Int] -> Int
unbinary = foldl (\acc bit -> (acc*2) + bit) 0

instruction :: ReadP Instruction
instruction = setMask +++ poke

poke :: ReadP Instruction
poke = do
  string "mem["
  address <- many1 (satisfy isNumber)
  string "] = "
  value <- many1 (satisfy isNumber)
  return $ Poke (read address, read value)

setMask :: ReadP Instruction
setMask = do
  string "mask = "
  mask <- many1 (satisfy isAlphaNum)
  return $ SetMask mask

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
