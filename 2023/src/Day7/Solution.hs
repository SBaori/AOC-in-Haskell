module Day7.Solution where
import Data.Char (digitToInt)
import Data.List (group, sort, sortBy, maximumBy)

parseInp :: [String] -> [(String,Int)]
parseInp = map ((\[c,v] -> (c,read v)) . words)

handType :: [String] -> Int
handType cardGroups = case minMaxLen cardGroups of
    (5,5) -> 7
    (1,4) -> 6
    (2,3) -> 5
    (1,3) -> 4
    (1,2) -> foldl (\maxCount g -> if length g == 2 then maxCount+1 else maxCount) 0 cardGroups + 1
    (1,1) -> 1
    where
        minMaxLen = foldl (\(minLen, maxLen) g -> (min minLen (length g), max maxLen (length g))) (6, 0)

compHandOnType :: String -> String -> (String -> [String]) -> Ordering
compHandOnType h1 h2 groupCard
    | handType cardGroups1 > handType cardGroups2 = GT
    | handType cardGroups1 < handType cardGroups2 = LT
    | otherwise = EQ
    where
        cardGroups1 = groupCard $ sort h1
        cardGroups2 = groupCard $ sort h2

compHandOnCard :: String -> String -> (Char -> Int) -> Ordering
compHandOnCard h1 h2 cardToNum = helper h1 h2
    where
        helper :: String -> String -> Ordering 
        helper [] [] = EQ
        helper (c1:c1s) (c2:c2s)
            | cardNum1 > cardNum2 = GT
            | cardNum1 < cardNum2 = LT
            | otherwise = helper c1s c2s
            where
                cardNum1 = cardToNum c1 
                cardNum2 = cardToNum c2

compHand :: String -> String -> (String -> [String]) -> (Char -> Int) -> Ordering
compHand h1 h2 groupCard cardMap
    | ordType /= EQ = ordType 
    | otherwise = ordCard
    where
        ordType = compHandOnType h1 h2 groupCard
        ordCard = compHandOnCard h1 h2 cardMap

-- Part 1

getPart1 :: [(String,Int)] -> Int
getPart1 = fst 
            . foldl (\(s,i) (_,v) -> (s+i*v,i+1)) (0,1) 
            . sortBy (\(h1,v1) (h2,v2) -> compHand h1 h2 group cardToNum)
    where
        cardToNum :: Char -> Int
        cardToNum card = case card of
            'A' -> 14
            'K' -> 13
            'Q' -> 12
            'J' -> 11
            'T' -> 10
            _ -> digitToInt card


-- Part 2

getPart2 :: [(String,Int)] -> Int
getPart2 = fst
            . foldl (\(s,i) (_,v) -> (s+i*v,i+1)) (0,1)
            . sortBy (\(h1,v1) (h2,v2) -> compHand h1 h2 virtualHandGroup cardToNum)
    where
        cardToNum :: Char -> Int
        cardToNum card = case card of
            'A' -> 13
            'K' -> 12
            'Q' -> 11
            'T' -> 10
            'J' -> 1
            _ -> digitToInt card

        virtualHandGroup :: String -> [String] 
        virtualHandGroup hand = group $ sort $ map (\x -> if x == 'J' then jChar else x) hand
            where
                jChar = head 
                        . maximumBy (\g1 g2 -> 
                            if head g2 == 'J' || (head g1 /= 'J' && length g1 > length g2) then 
                                GT 
                            else 
                                LT) 
                        . group $ hand
        
run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day7/input.txt"
    let part1 = getPart1 pinp
    let part2 = getPart2 pinp
    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2

