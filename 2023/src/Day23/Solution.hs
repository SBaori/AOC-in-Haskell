module Day23.Solution where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (guard, mplus)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (foldl')
import Data.Bits (Bits(testBit, setBit))

newtype Node = Node ((Int,Int), Int)
    deriving (Eq,Ord,Show)

newtype AdjList = AdjList (M.Map Node [(Node,Int)])

parseInp :: [String] -> M.Map (Int, Int) Char
parseInp inp = M.fromList $ concat [[((x,y), c) | (c,y) <- zip r [0..], c /= '#'] | (r,x) <- zip inp [0..]]

getAdjList :: S.Set (Int, Int) 
              -> [(Char, (Int, Int), Int, (Int, Int))] 
              -> M.Map (Int, Int) [((Int, Int), Int)] 
              -> (Int, Int)
              -> M.Map (Int, Int) Char 
              -> AdjList
getAdjList v [] m (rowLen,colLen) graph = AdjList m'
    where
        ml = M.toList m
        keyIdMap = M.fromList $ zip (map fst ml ++ [(rowLen-1, colLen-2)]) [1..]
        m' = M.fromList $ map (\(k,v) 
                            -> (Node (k, keyIdMap M.! k), map (\(n, d) -> (Node (n, keyIdMap M.! n), d)) v)) ml

getAdjList v ((val, c, i, (x,y)):cs) m dim@(rowLen,colLen) graph
    | x == rowLen - 1 = getAdjList (S.insert (x,y) v) cs m' dim graph
    | isNode = getAdjList v (map (\(val, _, _, p) -> (val, (x,y), 1, p)) getNextSteps ++ cs) m' dim graph
    | otherwise = getAdjList (S.insert (x,y) v) (getNextSteps ++ cs) m dim graph
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

getUpdatedAdjList :: AdjList -> AdjList
getUpdatedAdjList (AdjList l) = M.foldrWithKey updateMap (AdjList M.empty) l
    where
        updateMap :: Node -> [(Node, Int)] -> AdjList -> AdjList 
        updateMap k v (AdjList m) = AdjList $ M.unionWith (++) m $ M.fromList ((k,v) : vk)
            where
                vk = map (\(k', d) -> (k', [(k, d)])) v

getLongestPath :: Node -> AdjList -> Node -> Int
getLongestPath node (AdjList adjList) dest = head $ helper 0 node
    where
        helper :: Int -> Node -> [Int]
        helper v node@(Node (_, curruid))
            | node == dest = [0]
            | otherwise = do
                (adjNode@(Node (_, adjuid)), d) <- fromMaybe [] $ M.lookup node adjList
                guard (not $ testBit v adjuid)

                let adjDist = helper (setBit v curruid) adjNode

                pure $ maximum $ map (+ d) adjDist
                `mplus`
                pure (minBound :: Int)

getPart1 :: [String] -> Int
getPart1 inp = getLongestPath (Node ((0,1),1)) adjList (Node ((rowLen-1, colLen - 2),totNodes))
    where
        pinp = parseInp inp
        rowLen = length inp
        colLen = length $ head inp
        adjList@(AdjList a) = getAdjList S.empty [('.', (0,1), 0, (0,1))] M.empty (rowLen,colLen) pinp
        totNodes = M.size a + 1

getPart2 :: [String] -> Int
getPart2 inp = getLongestPath (Node ((0,1),1)) adjList (Node ((rowLen-1, colLen - 2),totNodes))
    where
        pinp = parseInp inp
        rowLen = length inp
        colLen = length $ head inp
        adjList@(AdjList a) = getUpdatedAdjList
                  $ getAdjList S.empty [('.', (0,1), 0, (0,1))] M.empty (rowLen,colLen) pinp
        totNodes = M.size a