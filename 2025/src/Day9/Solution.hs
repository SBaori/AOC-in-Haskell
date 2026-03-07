module Day9.Solution where

import Data.List (sortOn)
import Utils (printSolution)

parseInp :: String -> [(Int, Int)]
parseInp inp = linp
    where
        linp = map (\[a, b] -> (read a, read b))
                $ map words
                $ (map . map) (\x -> if x == ',' then ' ' else x)
                $ lines inp

getArea (x1, y1) (x2, y2) = (abs (x2-x1)+1) * (abs (y2-y1)+1)

getLargestEnclosed :: [(Int, Int)] -> [((Int, Int), (Int, Int))] -> Int
getLargestEnclosed points descAreas = getArea p1 p2
    where
        (p1, p2) = head $ dropWhile isInvalidRect descAreas

        lines = zip points (tail $ cycle points)

        isInvalidRect ((x1, y1), (x2, y2)) = any (\((lx1, ly1), (lx2, ly2))
                                                    -> not $
                                                            max lx1 lx2 <= left ||
                                                            right <= min lx1 lx2 ||
                                                            max ly1 ly2 <= top ||
                                                            bottom <= min ly1 ly2
                                                ) lines
            where
                left = min x1 x2
                right = max x1 x2
                top = min y1 y2
                bottom = max y1 y2

run :: IO ()
run = do
    points <- parseInp <$> readFile "src/Day9/input.txt"

    let descAreas = sortOn (\(p1, p2) -> negate $ getArea p1 p2)
                    $ [(p1, p2) | p1@(x1, y1) <- points, p2@(x2, y2) <- points, x1 /= x2 || y1 /= y2]

    let part1 = uncurry getArea $ head descAreas
    let part2 = getLargestEnclosed points descAreas

    printSolution(show part1, show part2)

