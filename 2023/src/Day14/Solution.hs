module Day14.Solution where

import Data.List (elemIndices, transpose, foldl', sort)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

-- Can be optimized further using Maps, but good enough when running executable
shift :: [Int] -> [Int] -> Bool -> [Int]
shift circles hashes flag
    | flag = [l+i | (l,h) <- zip hashes (tail hashes), (i,_) <- zip [1..] (filter (\x -> x>l && x<h) circles)]
    | otherwise = [h-i | (l,h) <- zip hashes (tail hashes), (i,_) <- zip [1..] (filter (\x -> x>l && x<h) circles)]

flipCoords :: [[Int]] -> Int -> [[Int]] 
flipCoords inds len = map (sort.snd) $ M.toList $ invCMap inds
    where
        initMap = M.fromList $ [(i,[]) | i <- [0..len-1]]
        invCMap = foldl' invRMap initMap . zip [0..]
        invRMap m (rNum,r) = foldl' (\prevM pos -> M.insertWith (++) pos [rNum] prevM) m r

calcLoad :: Int -> [[Int]] -> Int
calcLoad colLen = (foldl'.foldl') ((-) . (colLen+)) 0

getPart1 :: [String] -> Int
getPart1 inp = calcLoad (length inp) 
                $ map (\x -> shift (elemIndices 'O' x) ((-1):elemIndices '#' x ++ [length x]) True) pinp
    where
        pinp = transpose inp

getPart2 :: [String] -> Int
getPart2 inp = calcLoad (length inp) $ flipCoords (runRotations 0 M.empty rotations) rowNum
    where
        tinp = transpose inp
        rowNum = length inp
        colNum = length tinp
        hashesNS = map (\x -> (-1):elemIndices '#' x ++ [rowNum]) tinp
        hashesWE = map (\x -> (-1):elemIndices '#' x ++ [colNum]) inp

        circles = map (elemIndices 'O') inp

        rotate inds = let
                    north = zipWith (\ c h -> shift c h True) (flipCoords inds colNum) hashesNS
                    west = zipWith (\ c h -> shift c h True) (flipCoords north rowNum) hashesWE
                    south = zipWith (\ c h -> shift c h False) (flipCoords west colNum) hashesNS
                    east = zipWith (\ c h -> shift c h False) (flipCoords south rowNum) hashesWE
                in
                    east

        rotations = iterate rotate circles

        runRotations :: Int -> M.Map [[Int]] Int -> [[[Int]]] -> [[Int]]
        runRotations _ _ [] = []
        runRotations 1000000000 _ (g:_) = g
        runRotations i hash (g:gs)
            | isJust check = rotations !! (fromJust check + mod (1000000000 - i) (i- fromJust check))
            | otherwise = runRotations (i+1) (M.insert g i hash) gs
            where
                check = M.lookup g hash
