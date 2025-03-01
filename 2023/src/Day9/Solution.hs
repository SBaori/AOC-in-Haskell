module Day9.Solution where

parseInp :: [String] -> [[Int]]
parseInp = map (map read . words)

getTriangleSeq :: [Int] -> [[Int]]
getTriangleSeq = takeWhile (any (0 /=)) . iterate (\x -> zipWith (-) (tail x) x)

-- Part 1
getPart1 :: [[Int]] -> Int
getPart1 = foldl (\s ser -> let t = getTriangleSeq ser in s + nextTerm t) 0
    where
        nextTerm = foldl (\s ser -> s + last ser) 0

-- Part 2
getPart2 :: [[Int]] -> Int
getPart2 = foldl (\s ser -> let t = getTriangleSeq ser in s + prevTerm t) 0
    where
        prevTerm = foldr (\ser s -> head ser - s) 0

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day9/input.txt"

    let pinp = parseInp inp

    let part1 = getPart1 pinp
    let part2 = getPart2 pinp

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2