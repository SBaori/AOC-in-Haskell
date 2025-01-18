module Day22.Solution where

import Data.List.Split (splitOn)
import Data.List (sortBy, sort, foldl', maximumBy)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

parseInp :: [String] -> [([Int], [Int])]
parseInp = sortBy (getBrickHeightOrd fst)  .
            map ((\[x,y] -> (x,y)) . sort . map (map read . splitOn ",") . splitOn "~")

checkOverlap :: Ord a => ([a], [a]) -> ([a], [a]) -> Bool
checkOverlap ([xt1,yt1, _], [xt2, yt2, _]) ([xb1, yb1, _], [xb2, yb2, _]) =
    max xt1 xb1 <= min xt2 xb2 && max yt1 yb1 <= min yt2 yb2

getBrickTuple :: ([c], [f]) -> (c, c, c, f, f, f)
getBrickTuple ([x1, y1, z1], [x2, y2, z2]) = (x1, y1, z1, x2, y2, z2)

getBrickHeightOrd :: Ord a => (([a], [a]) -> [a]) -> ([a], [a]) -> ([a], [a]) -> Ordering
getBrickHeightOrd f b1 b2 = compare z1 z2
    where
        [_, _, z1] = f b1
        [_, _, z2] = f b2

getFallenBricks :: [([Int], [Int])] -> [([Int], [Int])] -> [([Int], [Int])]
getFallenBricks [] fallenBricks = sortBy (getBrickHeightOrd fst) fallenBricks
getFallenBricks (b:bs) fallenBricks = getFallenBricks bs (fallenBrick:fallenBricks)
    where
        ([x1, y1, z1], [x2, y2, z2]) = b

        support@([_, _, zs1], [_, _, zs2]) = maximumBy (getBrickHeightOrd snd) $
                                             ([0,0,0], [0,0,0]):[bf | bf <- fallenBricks, checkOverlap b bf]

        fallenBrick = ([x1, y1, 1 + max zs1 zs2], [x2, y2, (z2-z1) + 1 + max zs1 zs2])

-- TODO: two way pass to populate supportCover
getSupportCover :: [([Int], [Int])]
                    -> M.Map (Int, Int, Int, Int, Int, Int) ([([Int], [Int])], [([Int], [Int])])
                    -> [([Int], [Int])]
                    -> M.Map (Int, Int, Int, Int, Int, Int) ([([Int], [Int])], [([Int], [Int])])
getSupportCover [] s _ = s
getSupportCover (b:bs) supportCover seenBricks = getSupportCover bs supportCover'' (b:seenBricks)
    where
        ([_, _, zc1], [_, _, zc2]) = b

        supports = [sb | sb@([_, _, zs1], [_, _, zs2]) <- seenBricks,
                         checkOverlap b sb && (min zc1 zc2 == 1 + max zs1 zs2)]

        supportCover' = M.insertWith (\(ns, _) (os, oc) -> (ns ++ os, oc)) (getBrickTuple b) (supports, []) supportCover
        supportCover'' = foldl' (\scm s ->
                                    M.insertWith (\(_, nc) (os, oc) -> (os, nc ++ oc)) (getBrickTuple s) ([], [b]) scm) supportCover' supports

getPart1 :: [String] -> Int
getPart1 inp = length [sc | sc@(_, (_, covers)) <- M.toList supportCover,
                            all ((\c ->
                                    maybe False ((>1) . length . fst) (M.lookup c supportCover))
                                    . getBrickTuple) covers]
    where
        pinp = parseInp inp
        fallenBricks = getFallenBricks pinp []
        supportCover = getSupportCover fallenBricks M.empty []

getPart2 :: [String] -> Int
getPart2 inp = sum $ map (\b -> getDestroyable [b] (S.singleton b)) fallenBricks
    where
        pinp = parseInp inp
        fallenBricks = getFallenBricks pinp []
        supportCover = getSupportCover fallenBricks M.empty []

        getDestroyable :: [([Int], [Int])] -> S.Set ([Int], [Int]) -> Int
        getDestroyable q destroyable
            | null q' = S.size destroyable - 1
            | otherwise = getDestroyable q' destroyable'
            where
                destroyable' = S.union destroyable $ S.fromList q'

                q' = do
                    cd <- q

                    let cdt = getBrickTuple cd
                    let covers = maybe [] snd (M.lookup cdt supportCover)

                    cdc <- covers

                    let cdct = getBrickTuple cdc
                    let isValid = maybe False ((\s ->
                                                    S.isSubsetOf (S.fromList s) destroyable) . fst) (M.lookup cdct supportCover)

                    guard isValid
                    pure cdc


