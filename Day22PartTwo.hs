import Data.Text (splitOn, pack, unpack)
import Debug.Trace

type Game = [[Int]]
type Games = [Game]
type State = (Games, Game)

play :: State -> State
play (s, [[],p2]) = (s, [[],p2])
play (s, [p1,[]]) = (s, [p1,[]])
play (s, g@([p1,p2])) | g `elem` s = ([],[p1,[]])
play (s, g@[c1:r1,c2:r2]) = if ok then recurse else normal
  where ok = length r1 >= c1 && length r2 >= c2
        recurse = if rec1win then p1win else p2win
        rec1win = null p2r
        (_, [p1r,p2r]) = play ([], [take c1 r1, take c2 r2])
        normal = if c1 > c2 then p1win else p2win
        p1win = let e = [r1++[c1,c2],r2] in (g:s,e)
        p2win = let e = [r1,r2++[c2,c1]] in (g:s,e)

score :: Game -> Int
score = sum . mark . head . filter (not.null)
  where mark cs = zipWith (*) cs (ranks cs)
        ranks cs = reverse [1..length cs]

done :: State -> Bool
done (s,[p1,p2]) = null p1 || null p2

main = do
  contents <- readFile "sample.txt"
  let raw = splitOn (pack "\n\n") (pack $ contents)
      split = map (tail . splitOn (pack "\n")) raw
      decks :: [[Int]]
      decks = map (map (read . unpack)) split
  print $ score $ snd $ play $ last $ takeWhile (not.done) $ iterate play ([], decks)
