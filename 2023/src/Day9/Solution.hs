module Day9.Solution where

parseInp :: [String] -> [[Int]]
parseInp = map (map read . words)

getTriangleSeq :: [Int] -> [[Int]]
getTriangleSeq = takeWhile (any (0 /=)) . iterate (\x -> zipWith (-) (tail x) x)

-- Part 1
getPart1 :: [String] -> Int
getPart1 = foldl (\s ser -> let t = getTriangleSeq ser in s + nextTerm t) 0 . parseInp
    where
        nextTerm = foldl (\s ser -> s + last ser) 0

-- Part 2
getPart2 :: [String] -> Int
getPart2 = foldl (\s ser -> let t = getTriangleSeq ser in s + prevTerm t) 0 . parseInp
    where
        prevTerm = foldr (\ser s -> head ser - s) 0