module Day11.Solution where

import Data.List (elemIndices, nub, sort, group)

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

getPart1 :: [String] -> Int
getPart1 inp = sum [dist gp1 gp2 | gp1 <- expGalaxyPoints, gp2 <- expGalaxyPoints, gp1 < gp2]
    where
        galaxyPoints = getGalaxyPos inp 0 []
        expGalaxyPoints = expandPoints galaxyPoints 2

getPart2 :: [String] -> Int
getPart2 inp = sum [dist gp1 gp2 | gp1 <- expGalaxyPoints, gp2 <- expGalaxyPoints, gp1 < gp2]
    where
        galaxyPoints = getGalaxyPos inp 0 []
        expGalaxyPoints = expandPoints galaxyPoints 1000000