module Day21.Solution where

import qualified Data.Set as S
import Data.List (foldl')
import qualified Data.Map.Strict as M

push :: a1 -> (a2, [a1]) -> (a2, [a1])
push item (front,back) = (front, item:back)

pop :: ([a], [a]) -> (a, ([a], [a]))
pop ([], back) = (top, (items, []))
    where
        (top:items) = reverse back
pop (top:items, back) = (top, (items,back))

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x,y) = map (\(dx,dy) -> (x+dx, y+dy)) [(-1,0), (1,0), (0,1), (0,-1)]

parseInp ::[String] -> ((Int, Int), [(Int, Int)])
parseInp inp = row inp ((0,0), []) 0
    where
        row [] (startCoord,l) _ = (startCoord,l)
        row (r:rs) (startCoord,l) i = row rs (col r l startCoord 0) (i+1)
            where
                col [] l startCoord j = (startCoord,l)
                col (c:cs) l startCoord j
                    | c == '#' = col cs ((i,j):l) startCoord (j+1)
                    | c == 'S' = col cs l (i,j) (j+1)
                    | otherwise = col cs l startCoord (j+1)

bfs :: (Int, Int) -> S.Set (Int, Int) -> (Int, Int) -> M.Map (Int, Int) Int
bfs startCoord obstacles (w,h) = helper ([startCoord], []) (M.singleton startCoord 0)
    where
        helper :: ([(Int, Int)], [(Int, Int)]) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
        helper ([], []) v = v
        helper q@(f,b) v = helper (f'', b'') v'
            where
                (top, (f',b')) = pop q
                nbrs = getNeighbours top
                dist = v M.! top
                validNeighbours = filter (\(x,y) -> M.notMember (x,y) v && S.notMember (x,y) obstacles && x >= 0 && x < w && y >= 0 && y < h) nbrs
                (f'', b'') = foldl' (\(tf, tb) n -> push n (tf,tb)) (f',b') validNeighbours
                v' = foldl' (\tv n -> M.insert n (dist + 1) tv) v validNeighbours

getPart1 :: M.Map (Int, Int) Int -> Int
getPart1 = M.size . M.filter (\step -> even step && step < 65)

getPart2 :: M.Map (Int, Int) Int -> Int
getPart2 cellStepMap = (n+1)*(n+1)*oddFull + n*n*evenFull - (n+1)*oddCorners + n*evenCorners
    where
        n = 202300
        evenCorners = M.size $ M.filter (\step -> even step && step > 65) cellStepMap
        oddCorners = M.size $ M.filter (\step -> odd step && step > 65) cellStepMap
        evenFull = M.size $ M.filter even cellStepMap
        oddFull = M.size $ M.filter odd cellStepMap

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day21/input.txt"

    let pinp@(startCoord, obstacles) = parseInp inp
    let (rNum, cNum) = (length inp, length $ head inp)
    let cellStepMap = bfs startCoord (S.fromList obstacles) (rNum, cNum)

    let part1 = getPart1 cellStepMap
    let part2 = getPart2 cellStepMap

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2