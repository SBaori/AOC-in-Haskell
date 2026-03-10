module Day3.Solution where
import Data.Char (digitToInt)
import Utils (printSolution)

parseInp :: String -> [[Int]]
parseInp = (map.map) digitToInt . lines

getJoltage :: [Int] -> Int -> Int -> [(Int, Int)] -> [Int]
getJoltage [] _ _ joltage = reverse $ map snd joltage
getJoltage (j:js) len jLen joltages
    | stackLen == jLen = getJoltage js (len-1) jLen joltages
    | otherwise = getJoltage js (len-1) jLen ((stackLen+1, j):temp)
    where
        stackLen
            | null temp = 0
            | otherwise = fst $ head temp
        
        temp = dropWhile (\(l, j') -> j > j' && (jLen - l) < len) joltages

run :: IO ()
run = do
    pinp <- parseInp <$> readFile "src/Day3/input.txt"
    
    let part1 = sum
                $ map (read . concat . map show)
                $ map (\js -> getJoltage js (length js) 2 []) pinp
    
    let part2 = sum
                $ map (read . concat . map show)
                $ map (\js -> getJoltage js (length js) 12 []) pinp
    
    printSolution (show part1, show part2)