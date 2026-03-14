module Day5.Solution where

import Data.List.Split (splitOn, chunksOf)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust, mapMaybe)
import Data.List (sort, groupBy)
import Utils (printSolution)

mergeRanges [] _ acc =  reverse 
                        $ concat 
                        $ filter ((==1) . length)
                        $ groupBy (\(num1, t1) (num2, t2) ->
                                    num1 == num2 && t1 == 'b' && t2 == 'e'
                                ) acc

mergeRanges ((num, t):ps) [] acc = mergeRanges ps [(num, t)] acc
mergeRanges ((num, t):ps) stack@((num', t'):sts) acc
    | t == 'e' && null sts = mergeRanges ps sts ((num, t):(num', t'):acc)
    | t == 'e' = mergeRanges ps sts acc
    | otherwise = mergeRanges ps ((num,t):stack) acc

availableFresh queries rangeMap = length
                                    $ filter (\(_, t) -> t == 'b')
                                    $ mapMaybe (`IM.lookupLT` rangeMap) queries

allFresh :: [(Int, Char)] -> Int
allFresh = sum . map (\[(numb, _), (nume, _)] -> nume - numb + 1) . chunksOf 2 

parseInp :: String -> ([(Int, Char)], [Int])
parseInp inp = (ranges, queries)
    where
        inp' = splitOn [""] $ lines inp
        queries = map read $ inp' !! 1
        ranges = sort $ concatMap ((\[s, e] 
                                -> [(read s, 'b'), (read e, 'e')]
                            ) . splitOn "-"
                        ) (head inp')

run :: IO ()
run = do
    pinp <- parseInp <$> readFile "src/Day5/input.txt"

    let (ranges, queries) = pinp
    let mergedRanges = mergeRanges ranges [] []

    let part1 = availableFresh queries $ IM.fromList mergedRanges
    let part2 = allFresh mergedRanges

    printSolution(show part1, show part2)
    


