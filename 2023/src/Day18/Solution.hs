module Day18.Solution where
import Data.List.Split (splitOneOf)
import Data.List (groupBy, sortBy, foldl')
import Numeric (readHex)

parseInp :: [String] -> [[String]]
parseInp = map (filter (not . null) . splitOneOf " #()")

getVertices :: [[String]] -> [(Int, Int)]
getVertices = init . foldl' (\vrtcs@(v:_) [dir, len, _] -> (nextVertex dir len v):vrtcs) [(0,0)]
    where
        nextVertex dir len (x,y) = case dir of
            "R" -> (x+read len, y)
            "L" -> (x-read len, y)
            "U" -> (x, y-read len)
            "D" -> (x, y+read len)

getArea :: [(Int, Int)] -> Int
getArea vertices = div ((abs . sum) $ zipWith getDeterminant vertices (take len $ tail $ cycle vertices)) 2
    where
        len = length vertices
        getDeterminant (x1, y1) (x2, y2) = x1*y2 - x2*y1


getPerimeter :: [[String]] -> Int
getPerimeter = foldl' (\s [_, len, _] -> s + read len) 0

getPart1 :: [[String]] -> Int
getPart1 pinp = 1 + area + div perimeter 2
    where
        vertices = getVertices pinp
        area = getArea vertices
        perimeter = getPerimeter pinp

getPart2 :: [[String]] -> Int
getPart2 pinp = 1 + area + div perimeter 2
    where
        pinp' = map (\[_, _, color] -> [getDir $ last color, (show . fst . head) $ readHex $ init color, color]) pinp
            where
                getDir d = case d of
                    '0' -> "R"
                    '1' -> "D"
                    '2' -> "L"
                    '3' -> "U"

        vertices = getVertices pinp'
        area = getArea vertices
        perimeter = getPerimeter pinp'

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day18/input.txt"

    let part1 = getPart1 pinp
    let part2 = getPart2 pinp

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2