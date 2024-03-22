module Day12.Solution where
import Data.List.Split (splitOneOf)
import Data.List (group, elemIndex, inits, groupBy)
import Data.Maybe (fromJust, fromMaybe)

parseInp :: [String] -> [(String,[Int])]
parseInp = map (tup . splitOneOf " ,")
    where
        tup (sprgs:grps) = (sprgs,map read grps)

getCombCount :: [Char] -> [Int] -> Int 
getCombCount spr sprGrp = last $ last table
    where
        sprLen = length spr
        fstOprSpr = fromMaybe sprLen $ elemIndex '#' spr
        sprPrefixes = tail $ map reverse $ inits spr
        grpAllSprs = head . groupBy (\x y -> (x==y) || (x /= '.' && y /= '.'))

        table :: [[Int]]
        table = (replicate (fstOprSpr + 1) 1 ++ replicate (sprLen - fstOprSpr) 0) : [nextRow inp | inp <- zip sprGrp table]

        nextRow :: (Int,[Int]) -> [Int]
        nextRow (grp,prevRow) = let r = initCells ++ [nextCell grp inp | inp <- zip3 (drop grp r) prevRow (drop grp sprPrefixes)] in r
            where
                initCells = replicate grp 0 ++ 
                    if head prefix /= '.' && length (grpAllSprs prefix) == grp then 
                        [head prevRow] 
                    else 
                        [0]
                    where
                        prefix = sprPrefixes !! max 0 (grp-1)

        nextCell :: Int -> (Int,Int,[Char]) -> Int
        nextCell cg (prevCell,prevRowCell,prefix)
            | head prefix == '.' = prevCell 
            | head prefix == '?' = prevCell + if isValidPlace || isValidPlace2 then prevRowCell else 0
            | otherwise = if isValidPlace2 then prevRowCell else 0
            where
                unkownGrp = head $ group prefix 
                isValidPlace = (length unkownGrp > cg) || ((length prefix < (cg+1) || prefix !! cg /= '#') && length unkownGrp == cg)
                isValidPlace2 = (length (grpAllSprs prefix) >= cg) && (length prefix < (cg+1) || prefix !! cg /= '#')

-- Part 1
getPart1 :: [String] -> Int 
getPart1 = sum
            . map (uncurry getCombCount)
            . parseInp

-- Part 2
getPart2 :: [String] -> Int
getPart2 = sum . map (\(sprgs,grps) -> getCombCount (tail $ unfold 5 $ '?':sprgs) (unfold 5 grps))
            . parseInp
    where
        unfold n = concat . replicate n


