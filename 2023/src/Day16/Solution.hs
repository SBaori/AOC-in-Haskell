{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day16.Solution where
import Data.Maybe (isJust, fromJust)
import qualified Data.HashMap as M
import Data.List (foldl')

getLightMods :: [[Char]] -> M.Map (Int, Int) Char
getLightMods inp = M.fromList $ [((rNum,cNum),char) | (rNum,row) <- zip [0..] inp, (cNum,char) <- zip [0..] row, char /= '.']

getEnergizedCnt :: ((Int, Int), Char) -> M.Map (Int, Int) Char -> (Int,Int) -> M.Map (Int, Int) [Char] -> M.Map (Int, Int) [Char]
getEnergizedCnt ((x,y),dir) lightMods dim@(rNum,cNum) visited
    | x < 0 || x >= rNum || y < 0 || y >= cNum || checkVis = visited
    | isJust lightMod = case fromJust lightMod of
                            '\\' -> getEnergizedCnt (getNewPath '\\') lightMods dim (M.insertWith (++) (x,y) [dir] visited)
                            '/' -> getEnergizedCnt (getNewPath '/') lightMods dim (M.insertWith (++) (x,y) [dir] visited)
                            '|' -> case dir of
                                        'n' -> getEnergizedCnt ((x-1,y),dir) lightMods dim (M.insertWith (++) (x,y) [dir] visited)
                                        's' -> getEnergizedCnt ((x+1,y),dir) lightMods dim (M.insertWith (++) (x,y) [dir] visited)
                                        _ -> foldl' (\v splitPath -> getEnergizedCnt splitPath lightMods dim v) (M.insertWith (++) (x,y) [dir] visited) [((x-1,y),'n'), ((x+1,y),'s')]
                            '-' -> case dir of
                                        'e' -> getEnergizedCnt ((x,y+1),dir) lightMods dim (M.insertWith (++) (x,y) [dir] visited)
                                        'w' -> getEnergizedCnt ((x,y-1),dir) lightMods dim (M.insertWith (++) (x,y) [dir] visited)
                                        _ -> foldl' (\v splitPath -> getEnergizedCnt splitPath lightMods dim v) (M.insertWith (++) (x,y) [dir] visited) [((x,y+1),'e'),((x,y-1),'w')]
    | otherwise = getEnergizedCnt (getNewPath '.') lightMods dim (M.insertWith (++) (x,y) [dir] visited)
    where
        lightMod = M.lookup (x,y) lightMods
        
        checkVis = case M.lookup (x,y) visited of
                        Just dirs -> elem dir dirs
                        _ -> False

        getNewPath c = case c of
            '.' -> case dir of
                        'n' -> ((x-1,y),'n')
                        'e' -> ((x,y+1),'e')
                        's' -> ((x+1,y),'s')
                        'w' -> ((x,y-1),'w')
            '\\' -> case dir of
                        's' -> ((x,y+1),'e')
                        'w' -> ((x-1,y),'n')
                        'e' -> ((x+1,y),'s')
                        'n' -> ((x,y-1),'w')
            '/' -> case dir of
                        'e' -> ((x-1,y),'n')
                        's' -> ((x,y-1),'w')
                        'n' -> ((x,y+1),'e')
                        'w' -> ((x+1,y),'s')

getStart :: Int -> (Int,Int) -> [((Int,Int),Char)]
getStart part (rNum,cNum)
    | part == 1 = [((0,0),'e')]
    | otherwise = [((0,c),'s') | c <- [0..cNum-1]] ++
                  [((cNum-1,r),'w') | r <- [0..rNum-1]] ++
                  [((r,0),'e') | r <- [0..rNum-1]] ++
                  [((rNum-1,c),'n') | c <- [0..cNum-1]]

getPart1 :: M.Map (Int, Int) Char -> (Int,Int) -> Int
getPart1 lightMods (rNum, cNum) = maximum $ 
                                    map (\start -> M.size $ getEnergizedCnt start lightMods (rNum,cNum) M.empty) $ 
                                    getStart 1 (rNum,cNum)

-- TODO: Remove/cache all the visited edge points after every run.

getPart2 :: M.Map (Int, Int) Char -> (Int,Int) -> Int
getPart2 lightMods (rNum, cNum) = maximum $ 
                                    map (\start -> M.size $ getEnergizedCnt start lightMods (rNum,cNum) M.empty) $ 
                                    getStart 2 (rNum, cNum) 

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day16/input.txt"

    let lightMods = getLightMods inp
    let (rNum,cNum) = (length inp,length $ head inp)

    let part1 = getPart1 lightMods (rNum, cNum)
    let part2 = getPart2 lightMods (rNum, cNum)

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2