module Day17.Solution where

import qualified Data.Set as S
import Data.Char (digitToInt)
import qualified Data.Vector as V

parseInp :: [String] -> V.Vector Int
parseInp = V.fromList . map digitToInt . concat 

dijkstra :: S.Set (Int, Int, (Int, Int), Char) -> S.Set ((Int, Int), Char, Int) -> (Int,Int) -> (Int,Int) -> V.Vector Int -> Int
dijkstra pq visited (maxStep,minStep) (rn,cn) graph
    | (x,y) == (rn-1, cn-1) && steps >= minStep                                 = heatLoss 
    | S.member ((x,y), dir, steps) visited                                      = dijkstra pq' visited (maxStep,minStep) (rn,cn) graph
    | otherwise                                                                 = dijkstra updPq updVisited (maxStep,minStep) (rn,cn) graph
    where
        ((heatLoss, steps, (x,y), dir), pq') = S.deleteFindMin pq

        nextDirs = map (\(dir', (x',y'), s) -> ((graph V.! (cn*x' + y')) + heatLoss, s, (x',y'), dir'))
                $ filter (\(dir', (x',y'), s) -> x' >= 0
                                                && y' >= 0
                                                && x' < rn
                                                && y' < cn
                                                && S.notMember ((x',y'), dir', s) visited
                        ) getNextDirs

        updVisited = S.insert ((x,y), dir, steps) visited
        updPq = S.union pq' (S.fromList nextDirs)

        getNextDirs = case dir of
            'n' ->  [('n', (x-1,y), steps+1) | steps < maxStep] ++ concat [[('w', (x,y-1), 1), ('e', (x,y+1), 1)] | steps >= minStep]
            'e' ->  [('e', (x,y+1), steps+1) | steps < maxStep] ++ concat [[('n', (x-1,y), 1), ('s', (x+1,y), 1)] | steps >= minStep]
            's' ->  [('s', (x+1,y), steps+1) | steps < maxStep] ++ concat [[('e', (x,y+1), 1), ('w', (x,y-1), 1)] | steps >= minStep]
            _ ->    [('w', (x,y-1), steps+1) | steps < maxStep] ++ concat [[('s', (x+1,y), 1), ('n', (x-1,y), 1)] | steps >= minStep]


-- -- Part 1
getPart1 :: [String] -> Int
getPart1 inp = dijkstra pq S.empty (maxStep,minStep) (rn,cn) $ parseInp inp
    where
        rn = length inp
        cn = length $ head inp
        pq = S.singleton (0, 0, (0,0), 'e')
        visited = S.empty
        maxStep = 3
        minStep = 0

-- -- Part 2
getPart2 :: [String] -> Int
getPart2 inp = dijkstra pq S.empty (maxStep,minStep) (rn,cn) $ parseInp inp
    where
        rn = length inp
        cn = length $ head inp
        pq = S.fromList [(0, 0, (0,0), 'e'), (0, 0, (0,0), 's')]
        visited = S.empty
        maxStep = 10
        minStep = 4