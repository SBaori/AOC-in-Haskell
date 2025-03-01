module Day4.Solution where
import Data.List.Split (splitOn)
import Data.List ( elemIndices, foldl', tails)

parseInp :: [String] -> [[[Int]]]
parseInp = map (map (map (read :: String -> Int) . words) . splitOn "|" . concat . tail . splitOn ":")

matchCount :: [[Int]] -> Int
matchCount [winCards,normalCards] = foldl (\count card -> count + if not (null (elemIndices card winCards)) then 1 else 0) 0 normalCards

-- Part 1
sumPoints :: [[[Int]]] -> Double
sumPoints = foldl (\x y -> x + let r = matchCount y in if r == 0 then 0 else 2**(fromIntegral r - 1)) 0

-- Part 2
sumScratchCards :: [[[Int]]] -> Int
sumScratchCards pinp = sum $ cardCounts currMatchNums []
    where
        matchNums = map matchCount pinp
        currMatchNums = tail $ reverse $ tails $ reverse matchNums

        cardCounts :: [[Int]] -> [Int] -> [Int]
        cardCounts [] prevMatchNums = prevMatchNums
        cardCounts (cmn:cmns) prevMatchNums = cardCounts cmns (currCardCount:prevMatchNums)
            where
                currCardCount = snd $ foldl' (\(step,count) (cn,mn) -> if mn < step then 
                                                                            (step+1,count) 
                                                                        else 
                                                                            (step+1,count+cn)
                                            ) (1,1) $ zip prevMatchNums $ tail cmn

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day4/input.txt"
    let part1 = sumPoints pinp
    let part2 = sumScratchCards pinp
    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2