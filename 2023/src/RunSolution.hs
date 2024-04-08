module RunSolution where

import qualified Day7.Solution as D7
import qualified Day8.Solution as D8
import qualified Day9.Solution as D9
import qualified Day11.Solution as D11
import qualified Day12.Solution as D12
import qualified Day13.Solution as D13
import qualified Day15.Solution as D15
import qualified Day16.Solution as D16
import qualified Day6.Solution as D6
import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3
import qualified Day4.Solution as D4
import qualified Day5.Solution as D5
import qualified Day14.Solution as D14

-- TODO: Extract readfile IO to their respective files

runSolution :: String -> ([String] -> Int) -> ([String] -> Int) -> IO ()
runSolution inputPath getPart1 getPart2 = do
    contents <- readFile inputPath
    let inp = lines contents
        part1 = show $ getPart1 inp
        part2 = show $ getPart2 inp
    putStrLn $ "Part1: " ++ part1 ++ "\nPart2: " ++ part2

runSolution' :: String -> IO ()
runSolution' day = do
    contents <- readFile $ "src/Day" ++ day ++ "/input.txt"
    let 
        inp = lines contents
        (part1,part2) = case day of
            "1" -> (show $ D1.calibrationSum1 inp, show $ D1.calibrationSum2 inp)
            "2" -> (show $ D2.gameSum1 inp, show $ D2.gameSum2 inp)
            "3" -> (show $ D3.partNumSum inp, show $ D3.gearRatioSum inp)
            "4" -> (show $ D4.sumPoints inp, show $ D4.sumScratchCards inp)
            "5" -> (show $ D5.getLowestLoc1 inp, show $ D5.getLowestLoc2 inp)
            "6" -> (show $ D6.getWaysProd1 inp, show $ D6.getWaysProd2 inp)
            "7" -> (show $ D7.getPart1 inp, show $ D7.getPart2 inp)
            "8" -> (show $ D8.getPart1 inp, show $ D8.getPart2 inp)
            "9" -> (show $ D9.getPart1 inp, show $ D9.getPart2 inp)
            "11" -> (show $ D11.getPart1 inp, show $ D11.getPart2 inp)
            "12" -> (show $ D12.getPart1 inp, show $ D12.getPart2 inp)
            "13" -> (show $ D13.getPart1 inp, show $ D13.getPart2 inp)
            "14" -> (show $ D14.getPart1 inp, show $ D14.getPart2 inp)
            "15" -> (show $ D15.getPart1 inp, show $ D15.getPart2 inp)
            "16" -> (show $ D16.getPart1 inp, show $ D16.getPart2 inp)
            _ -> ("","")
        
    putStrLn $ "Part1: " ++ part1 ++ "\nPart2: " ++ part2

errorInp :: IO ()
errorInp = putStrLn "You're drunk"
