module Day25.Solution where

import Data.List.Split (splitOneOf)
import Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

parseInp :: [String] -> [[String]]
parseInp = map (filter (not . null) . splitOneOf ": ")

getAdjList :: [[String]] -> M.Map String [String] -> M.Map String [String]
getAdjList [] m = m
getAdjList (l:ls) m = getAdjList ls valueKeys
    where
        (n:ns) = l
        keyValues = foldl' (\tm v -> M.insertWith (++) n [v] tm) m ns
        valueKeys = foldl' (\tm v -> M.insertWith (++) v [n] tm) keyValues ns

getShortestPath :: String -> M.Map String Int -> M.Map String [String] -> S.Set (String, String) -> [String] -> S.Set (String, String)
getShortestPath node distMap adjList exclude path
    | dist == 0 = S.fromList $ zip (node:path) $ tail (node:path)
    | otherwise = getShortestPath parentNode distMap adjList exclude (node:path)
    where
        dist = distMap M.! node
        parentNode = head $ filter (\n -> not (isEdgePresent (node,n)) && (distMap M.! n) == dist - 1) $ adjList M.! node
        isEdgePresent (n1,n2) = S.member (n1,n2) exclude || S.member (n2,n1) exclude

getShortestDists :: [(String, Int)] -> M.Map String Int -> S.Set (String, String) -> M.Map String [String] -> M.Map String Int
getShortestDists [] v _ _ = v
getShortestDists q v exclude adjList = getShortestDists q' v' exclude adjList
    where
        tq = do
            (n,d) <- q
            map (\nbr -> (nbr,d+1)) $ filter (\n' -> not (isEdgePresent (n,n')) && (v M.! n') > d+1) $ adjList M.! n
        
        isEdgePresent (n1,n2) = S.member (n1,n2) exclude || S.member (n2,n1) exclude
        mq = M.fromList tq
        q' = M.toList mq
        v' = M.unionWith min v mq

getPart1 :: [[String]] -> Int
getPart1 pinp = getGroupProduct 3 S.empty start
    where
        adjList = getAdjList pinp M.empty
        start = fst $ M.findMin adjList
        dists = M.fromList $ map (\(k,v) -> if k == start then (k,0) else (k, maxBound :: Int)) $ M.toList adjList

        getGroupProduct :: Int -> S.Set (String, String) -> [Char] -> Int
        getGroupProduct 0 exclude start = group1 * group2
            where
                dists' = getShortestDists [(start,0)] dists exclude adjList
                group1 = length $ filter (\(_, d) -> d /= (maxBound :: Int)) $ M.toList dists'
                group2 = M.size adjList - group1
        getGroupProduct edgesLeft exclude start = getGroupProduct (edgesLeft-1) exclude' start 
            where
                dists' = getShortestDists [(start,0)] dists exclude adjList
                dest = fst . maximumBy (\(_, d1) (_,d2) -> compare d1 d2) $ M.toList dists'
                exclude' = S.union exclude $ getShortestPath dest dists' adjList exclude []

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day25/input.txt"
    let part1 = getPart1 pinp
    putStrLn $ "Part1: " ++ show part1