module Day6.Solution where
import Data.List.Split (splitOn)
import Data.List (transpose)
import Utils (printSolution)

evaluateExp :: [(String, [Int])] -> [Int]
evaluateExp = map (\(op, nums) -> if op == "+" then sum nums else product nums)

run :: IO ()
run = do
    linp <- lines <$> readFile "src/Day6/input.txt"

    let nums = init linp
    let operations = words $ last linp

    let part1 = sum
                $ evaluateExp
                $ map (\(op:nums) -> (op, map read nums))
                $ transpose
                $ operations : map words nums

    let part2 = sum 
                $ evaluateExp
                $ zip operations
                $ map concat
                $ splitOn [[]]
                $ map (map read . words)
                $ transpose nums

    printSolution(show part1, show part2)