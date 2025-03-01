module RunSolution where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3
import qualified Day4.Solution as D4
import qualified Day5.Solution as D5
import qualified Day6.Solution as D6
import qualified Day7.Solution as D7
import qualified Day8.Solution as D8
import qualified Day9.Solution as D9
import qualified Day10.Solution as D10
import qualified Day11.Solution as D11
import qualified Day12.Solution as D12
import qualified Day13.Solution as D13
import qualified Day14.Solution as D14
import qualified Day15.Solution as D15
import qualified Day16.Solution as D16
import qualified Day17.Solution as D17
import qualified Day18.Solution as D18
import qualified Day19.Solution as D19
import qualified Day20.Solution as D20
import qualified Day21.Solution as D21
import qualified Day22.Solution as D22
import qualified Day23.Solution as D23
import qualified Day24.Solution as D24
import qualified Day25.Solution as D25

runSolution :: String -> IO ()
runSolution day = do
    case day of
        "1" -> D1.run
        "2" -> D2.run
        "3" -> D3.run
        "4" -> D4.run
        "5" -> D5.run
        "6" -> D6.run
        "7" -> D7.run
        "8" -> D8.run
        "9" -> D9.run
        "10" -> D10.run
        "11" -> D11.run
        "12" -> D12.run
        "13" -> D13.run
        "14" -> D14.run
        "15" -> D15.run
        "16" -> D16.run
        "17" -> D17.run
        "18" -> D18.run
        "19" -> D19.run
        "20" -> D20.run
        "21" -> D21.run
        "22" -> D22.run
        "23" -> D23.run
        "24" -> D24.run
        "25" -> D25.run
        "_" -> errorInp

errorInp :: IO ()
errorInp = putStrLn "You're drunk"
