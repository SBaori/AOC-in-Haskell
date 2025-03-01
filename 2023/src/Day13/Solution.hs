module Day13.Solution where
import Data.List.Split (splitOn)
import Data.List (transpose)

parseInp :: [String] -> [[String]]
parseInp = splitOn [""]

getMirrorRowLoc :: [String] -> Int -> Int
getMirrorRowLoc pattern target = helper $ subtract 1 $ length $ head pattern
    where
        helper 0 = 0
        helper n
            | isValid n pattern = n
            | otherwise = helper (n-1)
        
        isValid n = (== target) . length . concatMap (filter not . checkMapping . splitAt n)

checkMapping :: (String,String) -> [Bool]
checkMapping (l1,l2) = zipWith (==) (reverse l1) l2

helper :: [[String]] -> Int -> Int
helper pinp target = foldl (\s (col,grid) -> s + if col == 0 then 
                                                    100 * getMirrorRowLoc (transpose grid) target 
                                                else 
                                                    col) 0 $ zip cols pinp
    where
        cols = map (`getMirrorRowLoc` target) pinp

getPart1 :: [[String]] -> Int
getPart1 = (`helper` 0)

getPart2 :: [[String]] -> Int
getPart2 = (`helper` 1)

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day13/input.txt"

    let part1 = getPart1 pinp 
    let part2 = getPart2 pinp

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2