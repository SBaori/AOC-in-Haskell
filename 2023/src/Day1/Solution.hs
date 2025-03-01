module Day1.Solution where

import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

-- Part 1
calibrationSum1 :: [String] -> Int
calibrationSum1 = foldl (\x y -> x + getCalibrationValue1 y) 0

getCalibrationValue1 :: String -> Int
getCalibrationValue1 s = read calValueStr :: Int
    where
        digitsStr = [dig | dig <- s, isDigit dig]
        calValueStr = [head digitsStr, last digitsStr]


-- Part 2
calibrationSum2 :: [String] -> Int
calibrationSum2 = foldl (\x y -> x + getCalibrationValue2 y 0 0) 0

getCalibrationValue2 :: String -> Int -> Int -> Int
getCalibrationValue2 [] t u = 10*t + if u == 0 then t else u
getCalibrationValue2 s@(c:cs) t u
    | t == 0 = case getValue s of
        (0,_) -> getCalibrationValue2 cs t u
        (v,l) -> getCalibrationValue2 l v u
    | otherwise = case getValue s of
        (0,_) -> getCalibrationValue2 cs t u
        (v,l) -> getCalibrationValue2 l t v
    where
        getValue s@(c:cs)
            | "one" `isPrefixOf` s = (1,drop 2 s)
            | "two" `isPrefixOf` s = (2, drop 2 s)
            | "three" `isPrefixOf` s = (3,drop 4 s)
            | "four" `isPrefixOf` s = (4, drop 4 s)
            | "five" `isPrefixOf` s = (5, drop 3 s)
            | "six" `isPrefixOf` s = (6, drop 3 s)
            | "seven" `isPrefixOf` s = (7, drop 4 s)
            | "eight" `isPrefixOf` s = (8, drop 4 s)
            | "nine" `isPrefixOf` s = (9, drop 3 s)
            | isDigit c = (digitToInt c,cs)
            | otherwise = (0,cs)

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day1/input.txt"
    let part1 = calibrationSum1 inp
    let part2 = calibrationSum2 inp
    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2
