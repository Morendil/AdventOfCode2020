import Common

main :: IO ()
main = do
  contents <- readFile "Day01.txt"
  putStrLn $ show $ hindThreeIs (map readInt . words $ contents)

hindsightIs :: [Integer] -> Integer
hindsightIs entries = answer
  where summing = [(a,b) | a <- entries, b <- entries, a+b == 2020]
        answer = case summing of
                  [(x,y),(a,b)] -> x * y
                  [] -> 0
                  _ -> toInteger $ length summing

hindThreeIs :: [Integer] -> Integer
hindThreeIs entries = answer
  where summing = [(a,b,c) | a <- entries, b <- entries, c <- entries, a+b+c == 2020]
        answer = case summing of
                  (x,y,z):_ -> x * y * z
                  _ -> 0
