module Day1.Solution where
import Data.List (foldl')
import Utils (printSolution)

parseInp :: String -> [Int]
parseInp = map (\(d:num) -> if d == 'R' then read num else -(read num)) . lines

getExactZeroes :: [Int] -> Int -> Int
getExactZeroes [] start = 0
getExactZeroes (n:ns) start
    | start == 0 = 1 + getExactZeroes ns (mod (start + dx + 100) 100)
    | otherwise = getExactZeroes ns (mod (start + dx + 100) 100)
    where
        dx
            | n > 0 = mod n 100
            | otherwise = -(mod (abs n) 100)

getAllZeroes :: [Int] -> Int -> Int
getAllZeroes [] start = 0
getAllZeroes (n:ns) start
    | start /= 0 && (next > 99 || next <= 0) = 1 + wrapAroundZeroes + getAllZeroes ns (mod (next + 100) 100)
    | otherwise = wrapAroundZeroes + getAllZeroes ns (mod (next + 100) 100)
    where
        wrapAroundZeroes = div (abs n) 100

        dx
            | n > 0 = mod n 100
            | otherwise = -(mod (abs n) 100)

        next = start + dx

run :: IO ()
run = do
    pinp <- parseInp <$> readFile "src/Day1/input.txt"

    let part1 = getExactZeroes pinp 50
    let part2 = getAllZeroes pinp 50

    printSolution(show part1, show part2)


