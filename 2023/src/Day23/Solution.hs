module Day23.Solution where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (guard, foldM, liftM2, mplus)
import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Data.List (foldl')

parseInp :: [String] -> M.Map (Int, Int) Char
parseInp inp = M.fromList $ concat [[((x,y), c) | (c,y) <- zip r [0..], c /= '#'] | (r,x) <- zip inp [0..]]

getAdjList :: S.Set (Int, Int) 
              -> [(Char, (Int, Int), Int, (Int, Int))] 
              -> M.Map (Int, Int) [((Int, Int), Int)] 
              -> Int 
              -> M.Map (Int, Int) Char 
              -> M.Map (Int, Int) [((Int, Int), Int)]
getAdjList v [] m rowLen graph = m
getAdjList v ((val, c, i, (x,y)):cs) m rowLen graph
    | x == rowLen - 1 = getAdjList (S.insert (x,y) v) cs m' rowLen graph
    | isNode = getAdjList v (map (\(val, _, _, p) -> (val, (x,y), 1, p)) getNextSteps ++ cs) m' rowLen graph
    | otherwise = getAdjList (S.insert (x,y) v) (getNextSteps ++ cs) m rowLen graph
    where
        m' = M.insertWith (++) c [((x,y), i)] m
        getConnections = case val of
            '>' -> [('.', c, i+1, (x,y+1))]
            'v' -> [('.', c, i+1, (x+1,y))]
            _        -> mapMaybe expand [((x+1,y), \v -> v == 'v' || v == '.'),
                                        ((x-1,y), (== '.')),
                                        ((x,y+1), \v -> v == '>' || v == '.'),
                                        ((x,y-1), (== '.'))]
            where
                expand (d, cond) = do
                    val <- M.lookup d graph
                    guard (cond val)
                    pure (val, c, i+1, d)

        isNode = all (\(val, _, _, _) -> val == '>' || val == 'v') getConnections
        getNextSteps = [(val, c, d, (x,y)) | (val, c, d, (x,y)) <- getConnections, S.notMember (x,y) v]

getUpdatedAdjList :: M.Map (Int,Int) [((Int,Int), Int)] -> M.Map (Int,Int) [((Int,Int), Int)]
getUpdatedAdjList = M.foldrWithKey updateMap M.empty
    where
        updateMap :: (Int,Int) -> [((Int,Int), Int)] -> M.Map (Int,Int) [((Int,Int), Int)] -> M.Map (Int,Int) [((Int,Int), Int)]
        updateMap k v m = M.unionWith (++) m $ M.fromList ((k,v) : vk)
            where
                vk = map (\(adj, d) -> (adj, [(k,d)])) v

getLongestPath :: S.Set (Int,Int) -> (Int,Int) -> M.Map (Int,Int) [((Int,Int), Int)] -> (Int,Int) -> Int
getLongestPath v node adjList dest = floor $ head $ helper v node
    where
        helper :: S.Set (Int,Int) -> (Int,Int) -> [Float]
        helper v node
            | node == dest = [0]
            | otherwise = do
                (adjNode, d) <- fromMaybe [] $ M.lookup node adjList
                guard (S.notMember adjNode v)

                let adjDist = helper (S.insert node v) adjNode
                let fracD = fromIntegral d

                pure $ maximum $ map (+ fracD) adjDist
                `mplus`
                pure (- (1 / 0))

getPart1 :: [String] -> Int
getPart1 inp = getLongestPath S.empty (0,1) adjList (rowLen-1, colLen - 2)
    where
        pinp = parseInp inp
        rowLen = length inp
        colLen = length $ head inp
        adjList = getAdjList S.empty [('.', (0,1), 0, (0,1))] M.empty rowLen pinp

getPart2 :: [String] -> Int
getPart2 inp = getLongestPath S.empty (0,1) adjList (rowLen-1, colLen - 2)
    where
        pinp = parseInp inp
        rowLen = length inp
        colLen = length $ head inp
        adjList = getUpdatedAdjList
                  $ getAdjList S.empty [('.', (0,1), 0, (0,1))] M.empty rowLen pinp