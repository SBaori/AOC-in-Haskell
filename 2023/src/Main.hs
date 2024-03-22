module Main where
import System.Environment (getArgs)
import RunSolution (runSolution')

main :: IO ()
main = do
    days <- getArgs
    mapM_ runSolution' days