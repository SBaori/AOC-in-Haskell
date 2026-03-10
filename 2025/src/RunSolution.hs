module RunSolution where

import qualified Day1.Solution as D1
import qualified Day2.Solution as D2
import qualified Day3.Solution as D3
import qualified Day4.Solution as D4
import qualified Day6.Solution as D6
import qualified Day5.Solution as D5
import qualified Day7.Solution as D7
import qualified Day8.Solution as D8
import qualified Day9.Solution as D9
import qualified Day10.Solution as D10
import qualified Day11.Solution as D11
import qualified Day12.Solution as D12

runSolution :: String -> IO ()
runSolution day = do
    case day of
        "1" -> D1.run
        "2" -> D2.run
        "3" -> D3.run
        "4" -> D4.run
        -- "5" -> D5.run
        "6" -> D6.run
        "7" -> D7.run
        "8" -> D8.run
        "9" -> D9.run
        "10" -> D10.run
        "11" -> D11.run
        "12" -> D12.run
        "_" -> errorInp

errorInp :: IO ()
errorInp = putStrLn "You're drunk"