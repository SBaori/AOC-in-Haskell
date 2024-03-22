module Day5.Solution where
import Data.List (groupBy)

parseInp :: [String] -> [[[Int]]]
parseInp = (map . map) (map read . words) . filter (\x -> head x /= ";") . groupBy (\x y -> x /= ";" && y /= ";")


getSeedLoc :: Int -> [[[Int]]] -> Int
getSeedLoc = foldl getMap

getMap :: Int -> [[Int]] -> Int
getMap key [] = key
getMap key ([ds,ss,l]:hms)
    | key >= ss && key < ss+l = ds-ss+key
    | otherwise = getMap key hms

getLowestLoc :: [[[Int]]] -> [Int] -> Int
getLowestLoc mapValues = foldl (\minLoc seed -> min minLoc (getSeedLoc seed mapValues)) (maxBound :: Int)

-- Part 1

getLowestLoc1 :: [String] -> Int
getLowestLoc1 inp = getLowestLoc mapValues seeds
    where
        pInp = parseInp inp
        mapValues = tail pInp
        seeds = head $ head pInp


-- Part 2

getLowestLoc2 :: [String] -> Int
getLowestLoc2 inp = helper seedRange (maxBound :: Int)
    where
        pInp = parseInp inp
        mapValues = tail pInp
        seedRange = head $ head pInp

        helper [] minLoc = minLoc
        helper (s:r:srs) minLoc = helper srs (min minLoc currLoc)
            where
                currLoc = getLowestLoc mapValues [s..s+r-1]


