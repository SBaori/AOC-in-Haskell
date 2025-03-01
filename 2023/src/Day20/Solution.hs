module Day20.Solution where

import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Control.Monad (guard, liftM2)
import Data.List (elem)

parseInp :: [String] -> (M.Map String [String], M.Map String Bool, M.Map String [String])
parseInp inp = (adjListMap, outState, conInvAdjListMap)
    where
        tempAdjList = (\[x,y] -> (x, splitOn ", " y)) . splitOn " -> " <$> inp

        newKey k = if head k == 'b' then k else tail k
        adjListMap = M.fromList $ (\(k,v) -> (newKey k, v)) <$> tempAdjList

        outState = M.insert "broadcaster" True $ foldr ((\k os -> M.insert (newKey k) False os) . fst) M.empty tempAdjList

        tempConInvAdjListMap = foldr (insertConjunction.fst) M.empty tempAdjList
            where
                insertConjunction k conInvAdjList= if head k /= '&' then conInvAdjList else M.insert (newKey k) [] conInvAdjList

        conInvAdjListMap = foldr updateConInvAdjListMap tempConInvAdjListMap tempAdjList
            where
                updateConInvAdjListMap (k,v) tempMap = foldr (M.adjust ([tail k] ++)) tempMap v

getNextPulse :: [(String, Bool)] -> M.Map String [String] -> M.Map String Bool -> M.Map String [String] -> (Int,Int) -> ([(String, Bool)], Int, Int, M.Map String Bool)
getNextPulse [] alm os cialm (h,l) = ([], h, l, os)
getNextPulse q adjListMap outStates conInvAdjListMap (high, low) = (q', high', low', outStates')
    where
        (node, inp) = head q
        outNodes = M.lookup node adjListMap
        outState = M.lookup node outStates
        conParents = M.lookup node conInvAdjListMap

        (high', low') = if inp then (high + 1, low) else (high, low+1)

        q' = tail q ++ fromMaybe [] (liftM2 (\o on -> map (,o) on) calculateOutput outNodes)

        outStates'
            | isNothing outNodes || node == "broadcaster" = outStates
            | otherwise = maybe outStates (\o -> M.insert node o outStates) calculateOutput

        calculateOutput
            | isNothing outNodes = Nothing
            | isNothing conParents = guard (not inp) >> not <$> outState
            | otherwise = not <$> foldr (liftM2 (&&) . (\p -> M.lookup p outStates)) (Just True) (concat conParents)



getPart1 :: (M.Map String [String], M.Map String Bool, M.Map String [String]) -> Int
getPart1 (adjListMap, outState, conInvAdjListMap) = getTotalHighLowProduct 1001 
    where
        getTotalHighLowProduct :: Int -> Int
        getTotalHighLowProduct numPress = (\(h,l,_) -> h*l) $
                                    last $
                                    take numPress $
                                    iterate (\(h,l,os) -> completeOnePress os (h,l)) (0,0, outState)

        completeOnePress :: M.Map String Bool -> (Int, Int) -> (Int, Int, M.Map String Bool)
        completeOnePress = helper [("broadcaster", False)]
            where
                helper [] os (h,l) = (h, l, os)
                helper q os (h,l) = helper q' os' (h', l')
                    where
                        (q', h', l', os') = getNextPulse q adjListMap os conInvAdjListMap (h,l)

getPart2 :: (M.Map String [String], M.Map String Bool, M.Map String [String]) -> Int
getPart2 (adjListMap, outState, conInvAdjListMap) = foldr lcm 1 $ getCycleLength rxGrandParent
    where
        rxParent = concatMap (\(p,c) -> ([p | elem "rx" c])) (M.toList adjListMap)
        rxGrandParent = concatMap (\rp -> concat $ M.lookup rp conInvAdjListMap) rxParent

        getCycleLength :: [String] -> [Int]
        getCycleLength nodes = populateCycleLengths outState M.empty 1
            where
                completeOnePress :: M.Map String Bool -> [String] -> ([String], M.Map String Bool)
                completeOnePress = helper [("broadcaster", False)]
                    where
                        helper [] os hn = (hn, os) 
                        helper q os hn = helper q' os' (hn ++ highNodes)
                            where
                                (q', _, _, os') = getNextPulse q adjListMap os conInvAdjListMap (0,0)
                                highNodes = [n | n <- nodes, fromJust $ M.lookup n os]

                populateCycleLengths :: M.Map String Bool -> M.Map String Int -> Int -> [Int]
                populateCycleLengths os highNodeCyclesMap numPress
                    | seenNodes == length nodes = map snd $ M.toList highNodeCyclesMap
                    | otherwise = populateCycleLengths os' highNodeCyclesMap' (numPress + 1) 
                    where
                        (highNodes ,os') = completeOnePress os []

                        highNodeCyclesMap' =    M.union highNodeCyclesMap $ 
                                                M.fromList $ 
                                                map (,numPress) highNodes

                        seenNodes = M.size highNodeCyclesMap

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day20/input.txt"

    let part1 = getPart1 pinp
    let part2 = getPart2 pinp

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2