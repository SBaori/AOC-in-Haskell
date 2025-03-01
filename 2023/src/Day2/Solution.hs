module Day2.Solution where
import Data.List.Split (splitOneOf) 


parseInp :: [String] -> [[(Int, Int, Int, Int)]]
parseInp = map (rgbTup . splitAt 1 . splitOneOf ":;")
    where
        getGameNum = read . last . words . head
        getRounds = map (map tail . splitOneOf ",")
        rgbTup (gnStr,rStr) = foldl (\(gn,r,g,b) y -> let [n,t] = words y in 
                                        case t of 
                                            "red" -> (gn,read n,g,b)
                                            "green" -> (gn,r,read n,b)
                                            "blue" -> (gn,r,g,read n)
                                    ) (getGameNum gnStr, 0,0,0) <$> getRounds rStr

-- Part 1

bagConfig :: (Int,Int,Int)
bagConfig = (12,13,14)

gameSum1 :: [[(Int, Int, Int, Int)]] -> Int
gameSum1 = calculateSum
    where
        (tr,tg,tb) = bagConfig
        calculateSum = foldl (\s g@((gn,_,_,_):_) -> if checkBounds g then s+gn else s) 0

        checkBounds :: [(Int,Int,Int,Int)] -> Bool
        checkBounds [] = True
        checkBounds ((gn,r,g,b):rs)
            | r <= tr && g <= tg && b <= tb = checkBounds rs
            | otherwise = False

-- Part 2

gameSum2 :: [[(Int, Int, Int, Int)]] -> Int
gameSum2 = calculateSum 
    where
        calculateSum = foldr (\game s -> let (r,g,b) = maxBagConfig game in r*g*b + s) 0
        maxBagConfig = foldr (\(_,r,g,b) (mr,mg,mb) -> (max r mr, max g mg, max b mb)) (0,0,0)

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day2/input.txt"
    let part1 = gameSum1 pinp
    let part2 = gameSum2 pinp
    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2