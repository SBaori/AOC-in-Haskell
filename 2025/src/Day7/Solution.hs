module Day7.Solution where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (foldl', findIndex, elemIndex)
import Data.Maybe (fromJust)
import Utils (printSolution)

solve :: [((Int, Int), Int)] -> (Int, Int) -> S.Set (Int, Int) -> Int -> (Int, Int)
solve queue (rLen, cLen) splitterMap splits
    | null queue' = (splits, sum $ map snd queue)
    | otherwise = solve queue' (rLen, cLen) splitterMap (currSplits + splits)
    where
        forwardBeams = do
            ((x, y), cnt) <- queue
            pure (S.member (x+1,y) splitterMap, ((x+1, y), cnt))

        currSplits = length $ filter fst forwardBeams

        queue' = splitBeams forwardBeams []

        splitBeams :: [(Bool, ((Int, Int), Int))] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
        splitBeams [] beams = M.toList $ M.fromListWith (+) $ [b | b@((x, y), _) <- beams, x < rLen, y < cLen]
        splitBeams ((isSplit, ((x,y), cnt)):fbs) beams
            | isSplit = splitBeams fbs (((x,y-1), cnt):((x,y+1), cnt):beams)
            | otherwise = splitBeams fbs (((x,y), cnt):beams)

run :: IO ()
run = do
    linp <- lines <$> readFile "src/Day7/input.txt"

    let splitterMap = S.fromList $ concat 
                        $ [[(rnum,cnum) | (cnum, c) <- zip [0..] r, c == '^'] | (rnum, r) <- zip [0..] linp]

    let startPos = ((1, fromJust $ elemIndex 'S' $ head linp), 1)

    let (part1, part2) = solve [startPos] (length linp, length $ head linp) splitterMap 0

    printSolution(show part1, show part2)