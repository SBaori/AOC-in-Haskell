module Day4.Solution where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Utils (printSolution)

parseInp :: String -> [((Int, Int), Char)]
parseInp inp = zip [(x,y) | x <- [0..length linp - 1], y <- [0..length (head linp)-1]] (concat linp)
    where
        linp = lines inp

swap :: (b, a) -> (a, b)
swap (a,b) = (b, a)

getAdjList :: [(Int, Int)] -> S.Set (Int, Int) -> [((Int, Int), [(Int, Int)])]
getAdjList paperRolls paperRollsSet = [(p, validAdjp) | p <- paperRolls, let validAdjp = filter (`S.member` paperRollsSet) $ getAdjPoints p]
    where
        getAdjPoints (x,y) = [(x+dx, y+dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /=0]


removeValidPaperRolls :: S.Set (Int, (Int, Int))
                        -> M.Map (Int, Int) Int
                        -> M.Map (Int, Int) [(Int, Int)]
                        -> Int
                        -> Int
removeValidPaperRolls prSet prNghbrCntMap adjMap removed
    | S.null prSet || adjCnt > 3 = removed
    | otherwise = removeValidPaperRolls prSet'' prNghbrCntMap' adjMap (removed+1)
    where
        ((adjCnt, point), prSet') = S.deleteFindMin prSet
       
        adjPoints = [(p, prNghbrCntMap M.! p - 1) | p <- adjMap M.! point, prNghbrCntMap M.! p > 3]

        prNghbrCntMap' = foldl' (\m (p, adjCnt) -> M.insert p adjCnt m) prNghbrCntMap adjPoints

        prSet'' = foldl' (\s (p, adjCnt) -> S.insert (adjCnt, p) s) prSet' 
                    $ filter ((<= 3) . snd) adjPoints

run :: IO ()
run = do
    pinp <- parseInp <$> readFile "src/Day4/input.txt"

    let paperRolls = map fst $ filter ((== '@') . snd) pinp
    let paperRollsSet = S.fromList paperRolls

    let adjList = getAdjList paperRolls paperRollsSet
    let validPaperRolls = filter ((< 4) . length . snd) adjList

    let part1 = length validPaperRolls

    let adjMap = M.fromList adjList

    let validPaperRollsCntSet = S.fromList
                                $ map (swap . (\(p, adjp) -> (p, length adjp))) validPaperRolls

    let paperRollsCntMap = M.fromList
                            $ map ((\(p, adjp) -> (p, length adjp))) adjList

    let part2 = removeValidPaperRolls validPaperRollsCntSet paperRollsCntMap adjMap 0

    printSolution(show part1, show part2)