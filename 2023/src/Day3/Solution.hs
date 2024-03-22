module Day3.Solution where

import Data.Char (isDigit)
import Data.List (groupBy, findIndex, findIndices, elemIndices,nub)

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
partNumSum :: [String] -> Int
partNumSum inp = foldl (\s (r,c,n) -> if isValidNum r c (length n) then
                                            s+read n
                                         else
                                            s
                          ) 0 numCoords
    where
        formatInp = parseInp inp
        numCoords = getNumCoords 0 [] formatInp
        rowLen = length inp
        colLen = length $ head inp

        isValidNum :: Int -> Int -> Int -> Bool
        isValidNum rowNum colNum currStrLen = isSymbol $ checkPoints rowNum colNum rowLen colLen currStrLen
            where
                prevRow = if rowNum > 0 then inp !! (rowNum-1) else []
                currRow = inp !! rowNum
                nextRow = if rowNum + 1 < rowLen then inp !! (rowNum+1) else []

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

gearRatioSum inp = f
    where
        formatInp = parseInp inp
        numCoords = getNumCoords 0 [] formatInp
        rowLen = length inp
        colLen = length $ head inp

        -- signature
        getStarCoords _ [] c = c
        getStarCoords rNum (r:rs) c = getStarCoords (rNum+1) rs (nc ++ c)
            where
                nc = [(rNum,cNum) | cNum <- elemIndices '*' r]

        starCoords = getStarCoords 0 inp []

        f = foldl (\s c -> s + g c) 0 starCoords
        g (r,c) = if length pn /= 2 then 0 else read (trd $ head pn)*read (trd $ last pn)
            where
                cp = checkPoints r c rowLen colLen 1
                h (r',c') = filter (\(r'', c'', n) -> r''==r' && c' >= c'' && c'<(c''+length n))
                pn = concat $ nub $ map (\x -> h x numCoords) cp



