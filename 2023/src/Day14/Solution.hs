module Day14.Solution where
import Data.List (elemIndices, transpose, findIndices)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

-- TODO: change [[Int]] to [(Int,Int)]

f circles hashes flag
    | flag = [l+i | (l,h) <- zip hashes (tail hashes), (i,_) <- zip [1..] (filter (\x -> x>l && x<h) circles)]
    | otherwise = [h-i | (l,h) <- zip hashes (tail hashes), (i,_) <- zip [1..] (filter (\x -> x>l && x<h) circles)]

-- getPart1 inp = (foldl.foldl) ((-) . (length inp+)) 0
--                 $ map (\x -> f (elemIndices 'O' x) ((-1):elemIndices '#' x ++ [length x]) True) pinp
--     where
--         pinp = transpose inp

g inp = helper 0 M.empty h
    where
        tinp = transpose inp
        rowNum = length inp
        colNum = length tinp
        hashesNS = map (\x -> (-1):elemIndices '#' x ++ [rowNum]) tinp
        hashesWE = map (\x -> (-1):elemIndices '#' x ++ [colNum]) inp
        flipCoords c len = [findIndices (elem i) c | i <- [0..len-1]]
        circles = map (elemIndices 'O') inp

        rotate c = let
                    north = zipWith (\ c h -> f c h True) (flipCoords c colNum) hashesNS
                    west = zipWith (\ c h -> f c h True) (flipCoords north rowNum) hashesWE
                    south = zipWith (\ c h -> f c h False) (flipCoords west colNum) hashesNS
                    east = zipWith (\ c h -> f c h False) (flipCoords south rowNum) hashesWE
                in
                    east

        h = iterate rotate circles        
        
        helper 1000000000 _ (g:gs) = g
        helper i hash (g:gs)
            | isJust check = h !! (fromJust check + mod (1000000000 - i) (i- fromJust check))
            | otherwise = helper (i+1) (M.insert g i hash) gs
            where
                check = M.lookup g hash

getPart2 inp = 0
    where
        tinp = transpose inp
        hashesNS = map (elemIndices '#') tinp
        hashesWE = map (elemIndices '#') inp