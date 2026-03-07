module Day10.Solution where

import Data.List.Split (splitOn)
import Data.List (foldl', subsequences)
import Data.Bits (setBit, xor, Bits (shiftR, testBit, shiftL, (.|.)))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Utils (printSolution)


parseInp :: String -> [(Int, Int, [Int], [Int])]
parseInp inp = inp''
    where
        inp' =  map ((\(lights:rest)
                        -> ((init . tail) lights, map (init . tail) $ init rest, (init . tail) $ last rest)) . words) (lines inp)

        inp'' = map (\(l, b, j)
                        -> (length l
                            ,foldl' (\n (l, i) -> if l == '#' then setBit n i else n) 0 $ zip (reverse l) [0..]
                            ,  map (foldl' (\n i -> setBit n (length l - i - 1)) 0 . map read . splitOn ",") b
                            , (map read . splitOn ",") j
                            )
                    ) inp'

getLightsButtonsMap :: [Int] -> IM.IntMap [(Int, [Int])]
getLightsButtonsMap buttons = IM.fromListWith (++) [(foldl' xor 0 bs, [(length bs, bs)]) | bs <- subsequences buttons]

getButtonPositions :: Int -> Int -> [Int] -> [Int]
getButtonPositions 0 _ acc = acc
getButtonPositions i buttonMask acc
    | testBit buttonMask 0 = getButtonPositions (i-1) (shiftR buttonMask 1) (1:acc)
    | otherwise = getButtonPositions (i-1) (shiftR buttonMask 1) (0:acc)

minJoltageButtonPresses :: Int -> [Int] -> [Int] -> IM.IntMap [(Int, [Int])] -> Int
minJoltageButtonPresses len joltage buttons joltageButtonsMap = helper joltage M.empty M.! joltage
    where
        buttonPosMap = IM.fromList [(b, getButtonPositions len b []) | b <- buttons]

        helper :: [Int] -> M.Map [Int] Int -> M.Map [Int] Int
        helper joltage cache
            | M.member joltage cache = cache
            | any (<0) joltage || null nextJoltages = cache
            | all (==0) joltage = M.insert joltage 0 cache
            | all even joltage = foldl' (\c (l, j) -> let j' = map (`div` 2) j; cache' = helper j' c
                                                      in M.insertWith min joltage (l + 2*M.findWithDefault 50000 j' cache') cache'
                                        ) cache nextJoltages

            | otherwise = foldl' (\c (l, j) -> let cache' = helper j c
                                               in M.insertWith min joltage (l + M.findWithDefault 50000 j cache'
                                ) cache') cache nextJoltages
            where
                lightsMask = foldl' (\m l -> shiftL m 1 .|. l) 0
                             $ map (\x -> mod x 2) joltage

                buttonPressesMask = IM.findWithDefault [] lightsMask joltageButtonsMap

                nextJoltages = [(l, foldl' (zipWith (-)) joltage $ map (buttonPosMap IM.!) bs)
                                            | (l, bs) <- buttonPressesMask]



run :: IO ()
run = do
    inp <- readFile "src/Day10/input.txt"

    let pinp = parseInp inp

    let allLightsButtonsMap = M.fromList $ map (\(_, _, b, _) -> (b, getLightsButtonsMap b)) pinp

    let part1 = sum $ map (\(len, lights, b, _) -> fst $ minimum ((allLightsButtonsMap M.! b) IM.! lights)) pinp
    let part2 = sum $ map (\(len, _, b, joltage) -> minJoltageButtonPresses len joltage b $ allLightsButtonsMap M.! b) pinp

    printSolution (show part1, show part2)


