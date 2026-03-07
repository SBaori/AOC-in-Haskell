module Utils where

printSolution :: (String, String) -> IO ()
printSolution (p1, p2) = putStrLn $ "Part1: " ++ p1 ++ "\n" ++ "Part2: " ++ p2