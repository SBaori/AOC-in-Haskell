module Day12.Solution where

import Data.List.Split (splitOn)
import Utils (printSolution)

parseInp :: [String] -> [(Int, [Int])]
parseInp inp = tiles
    where
        shapesTileCount = map (length . concatMap (filter (== '#')))
                 $ splitOn [""] $ init $ filter (\x -> null x || elem '#' x) inp

        tiles = map ((\(f:s)
                        -> ((\[a,b] -> read a * read b)
                                $ splitOn "x"
                                $ init f
                            , zipWith (*) shapesTileCount $ map read s
                            )
                    ) . words)
                $ filter (\x -> not (null x || elem '#' x || last x == ':')) inp

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day12/input.txt"

    let part1 = length
                $ filter id
                $ map (\(gridSize, shapeTileCount) -> gridSize >= sum shapeTileCount) pinp

    printSolution (show part1, "")

