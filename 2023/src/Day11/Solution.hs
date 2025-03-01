module Day11.Solution where

import Data.List (elemIndices, sort, group)

getGalaxyPos :: [String] -> Int -> [(Int,Int)] -> [(Int,Int)]
getGalaxyPos [] _ coords = coords
getGalaxyPos (r:rs) rNum coords = getGalaxyPos rs (rNum+1) (galaxyPos ++ coords)
    where
        galaxyPos = map (\x -> (rNum,x)) $ elemIndices '#' r

expandPoints :: [(Int, Int)] -> Int -> [(Int, Int)]
expandPoints points rate = map (\(x,y) -> (expx x, expy y)) points
    where
        rows = map head $ group $ sort $ map fst points
        cols = map head $ group $ sort $ map snd points
        expx x = x + (rate-1)*(x - 1 - length (takeWhile (<x) rows))
        expy y = y + (rate-1)*(y - 1 - length (takeWhile (<y) cols))

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y1-y2)

getPart1 :: [(Int, Int)] -> Int
getPart1 galaxyPoints = sum [dist gp1 gp2 | gp1 <- expGalaxyPoints, gp2 <- expGalaxyPoints, gp1 < gp2]
    where
        expGalaxyPoints = expandPoints galaxyPoints 2

getPart2 :: [(Int, Int)] -> Int
getPart2 galaxyPoints = sum [dist gp1 gp2 | gp1 <- expGalaxyPoints, gp2 <- expGalaxyPoints, gp1 < gp2]
    where
        expGalaxyPoints = expandPoints galaxyPoints 1000000

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day11/input.txt"

    let galaxyPoints = getGalaxyPos inp 0 []

    let part1 = getPart1 galaxyPoints
    let part2 = getPart2 galaxyPoints

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2