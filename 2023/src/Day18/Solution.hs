module Day18.Solution where
import Data.List.Split (splitOneOf)
import Data.List (groupBy, sortBy)

parseInp :: [String] -> [(Char, Int, String)]
parseInp = map ((\[dirDist,color] -> let (dir,dist) = splitAt 1 dirDist in (head dir, read dist, color)) . splitOneOf "()" . init)

getLoop (x,y) = helper (x,y) []
    where
        helper _ loop [] = loop
        helper (x,y) loop (i:is) = let (dir,dist,_) = i in
                                    case dir of
                                        'R' -> helper (x,y+dist) ([(x,y+i') | i' <- [dist,dist-1..1]] ++ loop) is
                                        'D' -> helper (x+dist,y) ([(x+i',y) | i' <- [dist,dist-1..1]] ++ loop) is
                                        'L' -> helper (x,y-dist) ([(x,y-i') | i' <- [dist,dist-1..1]] ++ loop) is
                                        'U' -> helper (x-dist,y) ([(x-i',y) | i' <- [dist,dist-1..1]] ++ loop) is

f rNum row loop  = walls
    where
        walls = groupBy (\(x1,_) (x2,_) -> x1==x2) $ sortBy (\(x1,_) (x2,_) -> if x1 > x2 then GT else LT) loop
        eqvWalls = filter (\w -> check (head w) (last w)) walls
        check s e = (\[(_,y1),(_,y2)] -> y1 /= y2) $ 
                    filter (\(x,y) -> (y == snd s && abs (x - fst s) == 1) || (y == snd e && abs (x - fst e) == 1)) loop