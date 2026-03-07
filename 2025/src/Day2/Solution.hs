module Day2.Solution where
import Data.List.Split (splitOn, chunksOf)
import Utils (printSolution)

parseInp :: String -> [(Int, Int)]
parseInp = map ((\[l, r] -> (l,r)) . map (read :: String -> Int) . splitOn "-") . splitOn ","

getAllInvalidNums :: [(Int, Int)] -> [[(Int, Int, Int)]]
getAllInvalidNums = map getInvalidNums
    where
        getInvalidNums (l, r) = [(num, length $ show num, splitLen) 
                                    | num <- [l..r], let splitLen = getInvalidSplitLen num, splitLen > 0]
        
        getInvalidSplitLen :: Int -> Int
        getInvalidSplitLen num = helper allSplits 
            where
                numStr = show num
                numLen = length numStr
                
                allSplits = [chunksOf l numStr 
                                            | l <- reverse [1 .. div numLen 2], mod numLen l == 0]

                helper :: [[String]] -> Int 
                helper [] = 0
                helper ((n:ns):splits)
                    | all (== n) ns = length n
                    | otherwise = helper splits

run :: IO ()
run = do
    pinp <- parseInp <$> readFile "src/Day2/input.txt"

    let invalidNums = concat $ getAllInvalidNums pinp

    let part1 = sum $ map (\(n, _, _) -> n) $ filter (\(_, l, sl) -> even l && div l 2 == sl) invalidNums
    let part2 = sum $ map (\(n, _, _) -> n) invalidNums

    printSolution(show part1, show part2)
