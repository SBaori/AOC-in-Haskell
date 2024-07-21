module Day17.Solution where

import qualified Data.Set as S
import Data.Char (digitToInt)

parseInp :: [String] -> [[Int]]
parseInp = (map.map) digitToInt

-- TODO: change graph :: [[Int]] to graph :: Vector (Vector Int)/graph :: Vector (Int,Int)
dijkstra :: S.Set (Int, (Int, Int), Char, Int) -> S.Set ((Int, Int), Char, Int) -> Int -> Int -> [[Int]] -> Int
dijkstra pq visited maxStep minStep graph
    | (x,y) == (length graph-1, length (head graph) - 1) && steps >= minStep    = heatLoss 
    | S.member ((x,y), dir, steps) visited                                      = dijkstra pq' visited maxStep minStep graph
    | otherwise                                                                 = dijkstra updPq updVisited maxStep minStep graph
    where
        ((heatLoss, (x,y), dir, steps), pq') = S.deleteFindMin pq

        nextDirs = map (\(dir', (x',y'), s) -> ((graph !! x' !! y') + heatLoss, (x',y'), dir', s))
                $ filter (\(dir', (x',y'), s) -> x' >= 0
                                                && y' >= 0
                                                && x' < length graph
                                                && y' < length (head graph)
                                                && S.notMember ((x',y'), dir', s) visited
                        ) getNextDirs

        updVisited = S.insert ((x,y), dir, steps) visited
        updPq = S.union pq' (S.fromList nextDirs)

        getNextDirs = case dir of
            'n' ->  [('n', (x-1,y), steps+1) | steps < maxStep] ++ concat [[('w', (x,y-1), 1), ('e', (x,y+1), 1)] | steps >= minStep]
            'e' ->  [('e', (x,y+1), steps+1) | steps < maxStep] ++ concat [[('n', (x-1,y), 1), ('s', (x+1,y), 1)] | steps >= minStep]
            's' ->  [('s', (x+1,y), steps+1) | steps < maxStep] ++ concat [[('e', (x,y+1), 1), ('w', (x,y-1), 1)] | steps >= minStep]
            _ ->    [('w', (x,y-1), steps+1) | steps < maxStep] ++ concat [[('s', (x+1,y), 1), ('n', (x-1,y), 1)] | steps >= minStep]


-- Part 1
getPart1 :: [String] -> Int
getPart1 = dijkstra pq visited maxStep minStep . parseInp
    where
        pq = S.singleton (0, (0,0), 'e', 0)
        visited = S.empty
        maxStep = 3
        minStep = 1

-- Part 2
getPart2 :: [String] -> Int
getPart2 = dijkstra pq visited maxStep minStep . parseInp
    where
        pq = S.fromList [(0, (0,0), 'e', 0), (0, (0,0), 's', 0)]
        visited = S.empty
        maxStep = 10
        minStep = 4