module Day24.Solution where

import Data.List.Split (splitOneOf, splitOn)
import qualified Data.Set as S
import qualified Data.Map as M

parseInp :: [String] -> [((Int, Int, Int), (Int, Int,Int))]
parseInp = map ((\[a,b] -> (a,b)) . map ((\[a,b,c] -> (read a,read b, read c)) . filter (not . null) . splitOneOf ", ") . splitOn "@")

getIntersectionAndTime :: ((Int, Int, Int), (Int, Int, Int)) -> ((Int, Int, Int), (Int, Int, Int)) -> (Double, Double, Double, Double)
getIntersectionAndTime s1 s2 = (xi, yi, t1, t2)
    where
        ((x1, y1, z1), (vx1, vy1, vz1)) = s1
        ((x2, y2, z2), (vx2, vy2, vz2)) = s2

        (dx1, dy1, dvx1, dvy1) = (fromIntegral x1, fromIntegral y1, fromIntegral vx1, fromIntegral vy1)
        (dx2, dy2, dvx2, dvy2) = (fromIntegral x2, fromIntegral y2, fromIntegral vx2, fromIntegral vy2)

        m1 = dvy1 / dvx1
        m2 = dvy2 / dvx2

        xi = (dy2 - dy1 + m1*dx1 - m2*dx2)/(m1-m2)
        yi = (m1*dy2 - m2*dy1 + m1*m2*(dx1 - dx2))/(m1-m2)

        t1 = (xi - dx1)/dvx1
        t2 = (xi - dx2)/dvx2

getPart1 :: [((Int, Int, Int), (Int, Int,Int))] -> Int
getPart1 pinp = length $ filter id [check s1 s2 | s1 <- pinp, s2 <- pinp, s1 > s2]
    where
        check :: ((Int, Int, Int), (Int, Int, Int)) -> ((Int, Int, Int), (Int, Int, Int)) -> Bool
        check s1 s2 = t1 >= 0
                    && t2 >= 0
                    && xi >= 2e14
                    && xi <= 4e14
                    && yi >= 2e14
                    && yi <= 4e14
            where
                (xi, yi, t1, t2) = getIntersectionAndTime s1 s2

-- TODO: Try minimizing conversion from Int to Frac and back
getPart2 :: [((Int, Int, Int), (Int, Int,Int))] -> Int
getPart2 pinp = rx + ry + rz
    where
        getXYIntersection :: [((Int, Int, Int), (Int, Int, Int))] -> [(Int, Int)] -> (Int, Int, Int, Int)
        getXYIntersection _ [] = (0,0,0,0)
        getXYIntersection stones ((rvx, rvy):sps)
            | S.size intersection == 1 = head $ map (\(x,y) -> (x,y,rvx,rvy)) $ S.toList intersection
            | otherwise = getXYIntersection stones sps
            where
                relStones = map (\(p, (vx, vy, vz)) -> (p, (vx-rvx, vy-rvy,vz))) stones
                intersection = S.fromList
                                $ map (\(x,y,t1,t2) -> (x,y)) 
                                $ [(\(x,y,t1,t2) -> (round x,round y,t1,t2)) $ getIntersectionAndTime s1 s2
                                    | s1 <- relStones, s2 <- relStones, s1 > s2]

        (rx, ry, rvx, rvy) = getXYIntersection (take 4 pinp) $ [(vx, vy) | vx <- [-500..500], vy <- [-500..500]]

        rz = round $ (t1 * (fromIntegral z2) + t1 * t2 * (fromIntegral (vz2 - vz1)) - t2 * (fromIntegral z1)) / (t1 - t2)
            where
                [((x1,_,z1), (vx1,_,vz1)), ((x2,_,z2),(vx2,_,vz2))] = take 2 pinp

                t1 = fromIntegral (rx-x1) / fromIntegral (vx1-rvx)
                t2 = fromIntegral (rx-x2) / fromIntegral (vx2-rvx)

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day24/input.txt"
    
    let part1 = getPart1 pinp
    let part2 = getPart2 pinp

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2