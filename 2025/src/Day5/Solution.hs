module Day5.Solution where

import Data.List.Split (splitOn, chunksOf)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (isJust, mapMaybe)
import Data.List (sort, groupBy)

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

part1 queries rangeMap = length
                            $ filter (\(_, t) -> t == 'b')
                            $ mapMaybe (`IM.lookupLT` rangeMap) queries

part2 :: [(Int, Char)] -> Int
part2 = sum . map (\[(numb, _), (nume, _)] -> nume - numb + 1) . chunksOf 2 

parseInp :: String -> ([(Int, Char)], [Int])
parseInp inp = (ranges, queries)
    where
        inp' = splitOn [""] $ lines inp
        queries = map read $ inp' !! 1
        ranges = sort $ concatMap ((\[s, e] 
                                -> [(read s, 'b'), (read e, 'e')]
                            ) . splitOn "-"
                        ) (head inp')


