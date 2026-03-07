module Day11.Solution where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (guard)
import Utils (printSolution)

parseInp :: [String] -> [([Char], [[Char]])]
parseInp = map ((\(f:s) -> (init f, s)) . words)

getPathCounts :: String -> String -> S.Set String -> M.Map String [String] -> M.Map String Int -> M.Map String (String, Int)
getPathCounts start end junctions adjMap inDegreeMap = solve [start] inDegreeMap $ M.singleton start ("", 1)
    where
        solve :: [String] -> M.Map String Int -> M.Map String (String, Int) -> M.Map String (String, Int)
        solve q inDegreeMap pathCntMap
            | S.member end nodeIdSet = pathCntMap'
            | otherwise = solve q' inDegreeMap' pathCntMap'
            where
                inDegreeMap' = foldl' (\im (nodeId, _)
                                            -> M.insertWith (flip (-)) nodeId 1 im) inDegreeMap childParentConnections

                pathCntMap' = foldl' (\pcm (nodeId, pathNodeIdCnt)
                                        -> M.insertWith insert nodeId pathNodeIdCnt pcm) pathCntMap childParentConnections
                    where
                        insert (nv, cnt1) (ov, cnt2)
                            | nv == ov = (nv, cnt1 + cnt2)
                            | length nv > length ov = (nv, cnt1)
                            | length nv < length ov = (ov, cnt2)
                            | otherwise = (nv ++ ov, cnt1 + cnt2)

                nodeIdSet = S.fromList [nodeId | (nodeId, _) <- childParentConnections, inDegreeMap' M.! nodeId == 0]
                q' = S.toList nodeIdSet

                childParentConnections = do
                    nodeId <- q
                    adjNodeId <- adjMap M.! nodeId

                    let (pathNodeId, cnt) = pathCntMap M.! nodeId
                    let nodeId'
                            | S.member nodeId junctions = nodeId
                            | otherwise = ""

                    pure (adjNodeId, (nodeId' ++ pathNodeId, cnt))

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day11/input.txt"
    let pinp = parseInp inp

    let start = "svr"
    let end = "out"

    let adjMap = M.fromList $ (end, []):pinp
    let reverseAdjMap = (start, []):[(child, node) | (node, children) <- pinp, child <- children]
    let inDegreeMap = M.fromListWith (+) $ map (\(f,s) -> (f,1)) reverseAdjMap

    let pathCounts1 = getPathCounts start end (S.fromList ["you"]) adjMap inDegreeMap
    let pathCountThroughYou = snd $ pathCounts1 M.! end
    let pathCountEndingYou = snd $ pathCounts1 M.! "you"

    let part1 = div pathCountThroughYou pathCountEndingYou

    let pathCounts2 = getPathCounts start end (S.fromList ["dac", "fft"]) adjMap inDegreeMap

    let part2 = snd $ pathCounts2 M.! end

    printSolution (show part1, show part2)
