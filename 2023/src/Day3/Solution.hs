module Day3.Solution where

import Data.Char (isDigit)
import Data.List (groupBy, elemIndices,nub)

trd :: (a, b, c) -> c
trd (_,_,c) = c

parseInp :: [String] -> [[String]]
parseInp = map (groupBy (\x y -> isDigit x && isDigit y))

getNumCoords :: Int -> [(Int,Int,String)] -> [[String]] -> [(Int,Int,String)]
getNumCoords _ l [] = l
getNumCoords i l (r:rs) = getNumCoords (i+1) (helper i 0 r l) rs
    where
        helper _ _ [] numCoords = numCoords
        helper rNum cNum (e:es) numCoords
            | isDigit $ head e = helper rNum (cNum+length e) es ((rNum,cNum,e):numCoords)
            | otherwise = helper rNum (cNum+1) es numCoords

checkPoints :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
checkPoints rowNum colNum rowLen colLen elemLen =
            [(rowNum-1,cn) | rowNum > 0, cn <- [colNum-1 .. colNum + elemLen], cn > 0, cn < colLen] ++
            [(rowNum,colNum-1) | colNum > 0] ++ [(rowNum,colNum+elemLen) | (colNum+elemLen)<colLen] ++
            [(rowNum+1,cn) | rowNum+1<rowLen, cn <- [colNum-1 .. colNum+elemLen], cn > 0, cn < colLen]
-- Part 1
-- TODO: convert foldl to parallel list comprehension
partNumSum :: [String] -> [[String]] -> [(Int, Int, String)] -> (Int,Int) -> Int
partNumSum inp pinp numCoords (rNum, cNum) = foldl (\s (r,c,n) -> if isValidNum r c (length n) then
                                            s+read n
                                         else
                                            s
                          ) 0 numCoords
    where
        isValidNum :: Int -> Int -> Int -> Bool
        isValidNum rowNum colNum currStrLen = isSymbol $ checkPoints rowNum colNum rNum cNum currStrLen
            where
                prevRow = if rowNum > 0 then inp !! (rowNum-1) else []
                currRow = inp !! rowNum
                nextRow = if rowNum + 1 < rNum then inp !! (rowNum+1) else []

                isSymbol :: [(Int,Int)] -> Bool
                isSymbol [] = False
                isSymbol (cp:cps)
                    | (rNum == (rowNum-1) && (psym /= '.' && not (isDigit psym))) ||
                      (rNum == rowNum && (csym /= '.' && not (isDigit csym))) ||
                      (rNum == (rowNum+1) && (nsym /= '.' && not (isDigit nsym))) = True
                    | otherwise = isSymbol cps
                    where
                        rNum = fst cp
                        cNum = snd cp
                        psym = prevRow !! cNum
                        csym = currRow !! cNum
                        nsym = nextRow !! cNum

-- Part 2

gearRatioSum inp pinp numCoords (rNum, cNum) = f
    where
        -- signature
        getStarCoords _ [] c = c
        getStarCoords rNum (r:rs) c = getStarCoords (rNum+1) rs (nc ++ c)
            where
                nc = [(rNum,cNum) | cNum <- elemIndices '*' r]

        starCoords = getStarCoords 0 inp []

        f = foldl (\s c -> s + g c) 0 starCoords
        g (r,c) = if length pn /= 2 then 0 else read (trd $ head pn)*read (trd $ last pn)
            where
                cp = checkPoints r c rNum cNum 1
                h (r',c') = filter (\(r'', c'', n) -> r''==r' && c' >= c'' && c'<(c''+length n))
                pn = concat $ nub $ map (\x -> h x numCoords) cp

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day3/input.txt"

    let pinp = parseInp inp
    let (rNum, cNum) = (length inp, length $ head inp)
    let numCoords = getNumCoords 0 [] pinp

    let part1 = partNumSum inp pinp numCoords (rNum, cNum)
    let part2 = gearRatioSum inp pinp numCoords (rNum, cNum)

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2

