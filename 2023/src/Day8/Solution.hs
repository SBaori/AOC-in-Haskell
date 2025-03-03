module Day8.Solution where

import Data.List.Split (splitOneOf)
import qualified Data.Map as M

parseMap :: [String] -> [(String, [String])]
parseMap =
    map
        ( (\[p, l, r] -> (p, [l, r]))
            . concatMap words
            . splitOneOf "(=,)"
        )

hash :: [String] -> M.Map String [String]
hash = M.fromList . parseMap . drop 2

dirs :: [String] -> [Int]
dirs = cycle . map (\x -> if x == 'L' then 0 else 1) . head

-- Apparently, all locations ending with A(starting points) end up with only one location ending with 'Z'(ending points), so this works.
-- Yes, you were supposed to figure this out on your own.
getCount :: String -> [Int] -> Int -> M.Map String [String] -> Int
getCount loc (d : ds) count hash
    | last loc == 'Z' = count
    | otherwise = getCount ((hash M.! loc) !! d) ds (count + 1) hash

-- Part 1
getPart1 :: M.Map String [String] -> [Int] -> Int
getPart1 h d = getCount "AAA" d 0 h

-- Part 2
getPart2 :: M.Map String [String] -> [Int] -> [(String, [String])] -> Int
getPart2 h d maps = foldl (\s loc -> let r = getCount loc d 0 h in floor $ fromIntegral (r * s) / fromIntegral (gcd r s)) 1 startLocs
  where
    startLocs = map fst $ filter (\(loc, _) -> last loc == 'A') maps

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day8/input.txt"
    let h = hash inp
    let d = dirs inp
    let maps = parseMap $ drop 2 inp

    let part1 = getPart1 h d
    let part2 = getPart2 h d maps

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2