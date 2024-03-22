module Day6.Solution where
import Data.List.Split (splitOn)

getWinCount :: (Int, Int) -> Int
getWinCount (t,d) = 2*(r-l+1) - if mod t 2 == 1 then 0 else 1 
    where
        r = div t 2
        l = getLowerInd 0 r (\x -> x*(t-x)) d

        getLowerInd :: Int -> Int -> (Int -> Int) -> Int -> Int
        getLowerInd l r cond val
            | l >= r = if cond l <= val then l+1 else l
            | cond mid > val = getLowerInd l (mid-1) cond val
            | otherwise = getLowerInd (mid+1) r cond val
            where
                mid = l + div (r-l) 2

-- Part 1

parseInp1 :: [String] -> [(Int,Int)]
parseInp1 inp = zip (head p) (last p)
    where
        p = map (concatMap (map read . words) . tail . splitOn ":") inp

getWaysProd1 :: [String] -> Int
getWaysProd1 = foldl (\mul r -> mul*getWinCount r) 1 . parseInp1

-- Part 2

parseInp2 :: [String] -> (Int,Int)
parseInp2 inp = (read $ head p,read $ last p)
    where
        p = map (concatMap (concat . words) . tail . splitOn ":") inp
    
getWaysProd2:: [String] -> Int
getWaysProd2 = getWinCount . parseInp2