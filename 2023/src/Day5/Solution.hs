module Day5.Solution where
import Data.List (groupBy, foldl', sortBy, minimumBy)

parseInp :: [String] -> [[[Int]]]
parseInp = (map . map) (map read . words) . filter (\x -> head x /= ";") . groupBy (\x y -> x /= ";" && y /= ";")

getMap :: Int -> [[Int]] -> Int
getMap key [] = key
getMap key ([ds,ss,l]:hms)
    | key >= ss && key < ss+l = ds-ss+key
    | otherwise = getMap key hms

-- Part 1

getSeedLoc :: Int -> [[[Int]]] -> Int
getSeedLoc = foldl getMap

getLowestLoc :: [[[Int]]] -> [Int] -> Int
getLowestLoc mapValues = foldl (\minLoc seed -> min minLoc (getSeedLoc seed mapValues)) (maxBound :: Int)

getPart1 :: [[[Int]]] -> [Int] -> Int
getPart1 = getLowestLoc

-- Part 2

getSeedRangeLoc :: [[Int]] -> [[[Int]]] -> [[Int]]
getSeedRangeLoc = foldl' helper 
    where
        helper :: [[Int]] -> [[Int]] -> [[Int]]
        helper ranges maps = concatMap (\range -> getRangeMap range sortMaps) ranges
            where
                sortMaps = sortBy (\[_,x,_] [_,y,_] -> if x > y then GT else LT) maps

getRangeMap :: [Int] -> [[Int]] -> [[Int]]
getRangeMap [start, len] maps = helper (start-1) maps []
    where
        helper :: Int -> [[Int]] -> [[Int]] -> [[Int]]
        helper prevInpMapEnd [] res = endVoid ++ res
            where
                endVoid = [[prevInpMapEnd + 1, start+len - 1 - prevInpMapEnd] | prevInpMapEnd < (start + len - 1)]
        helper prevInpEnd (mp@[_, inpStart, mapLen]:mps) res
            | inpStart > (start+len-1) = endVoid ++ res
            | (inpStart + mapLen - 1) < start = helper prevInpEnd mps res
            | otherwise = helper (inpStart+mapLen-1) mps (void ++ [mapStart, mapEnd-mapStart+1]:res)
            where
                void = [[prevInpEnd + 1, inpStart-prevInpEnd-1] | (inpStart-prevInpEnd) > 1]
                endVoid = [[prevInpEnd + 1, start+len - 1 - prevInpEnd] | prevInpEnd < (start + len - 1)]
                mapStart = getMap (max start inpStart) [mp] 
                mapEnd = getMap (min (start+len-1) (inpStart+mapLen-1)) [mp] 

getLowestLoc2 :: [[Int]] -> [[[Int]]] -> Int
getLowestLoc2 seedRanges maps = head $ minimumBy (\r1 r2 -> if head r1 > head r2 then GT else LT) $ getSeedRangeLoc seedRanges maps

-- Part 2

getPart2 :: [[[Int]]] -> [Int] -> Int
getPart2 mapValues seeds = getLowestLoc2 seedRanges mapValues 
    where
        seedRanges = helper seeds
            where
                helper :: [Int] -> [[Int]]
                helper [] = []
                helper (seed:range:seeds) = [seed,range]:helper seeds

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day5/input.txt"
    let mapValues = tail pinp
    let seeds = head $ head pinp
    let part1 = getPart1 mapValues seeds
    let part2 = getPart2 mapValues seeds
    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2


