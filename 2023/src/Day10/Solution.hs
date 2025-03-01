module Day10.Solution where
import qualified Data.Map as M
import Data.List (elemIndex, elemIndices)
import Data.Maybe (isNothing)

pipes :: M.Map Char (Char, Char)
pipes = M.fromList [('|', ('n','s')), ('-', ('e','w')), ('L', ('n','e')), ('J',('n','w')), ('7',('s','w')), ('F', ('s','e')), ('.', ('.','.'))]

getNextDir :: Char -> Char -> Char
getNextDir dir pipe = case dir of
    'n' -> case (d1,d2) of
        (nd,'s') -> nd
        ('s',nd) -> nd
        _ -> '.'
    's' -> case (d1,d2) of
        (nd,'n') -> nd
        ('n',nd) -> nd
        _ -> '.'
    'e' -> case (d1,d2) of
        (nd,'w') -> nd
        ('w',nd) -> nd
        _ -> '.'
    'w' -> case (d1,d2) of
        (nd,'e') -> nd
        ('e',nd) -> nd
        _ -> '.'
    _ -> '.'
    where
        (d1,d2) = pipes M.! pipe

getPath :: (Int, Int) -> [[Char]] -> [(Char, (Int,Int))] -> Char -> [(Char, (Int,Int))]
getPath (x,y) tiles loop dir
    | tiles !! x !! y == 'S' = (tiles !! x !! y, (x,y)):loop
    | dir == 'n' = getPath (x-1,y) tiles ((tiles !! x !! y, (x,y)):loop) (getNextDir dir (tiles !! (x-1) !! y))
    | dir == 's' = getPath (x+1,y) tiles ((tiles !! x !! y, (x,y)):loop) (getNextDir dir (tiles !! (x+1) !! y))
    | dir == 'e' = getPath (x,y+1) tiles ((tiles !! x !! y, (x,y)):loop) (getNextDir dir (tiles !! x !! (y+1)))
    | otherwise = getPath (x,y-1) tiles ((tiles !! x !! y, (x,y)):loop) (getNextDir dir (tiles !! x !! (y-1)))

getStartCoord :: Int -> [String] -> (Int,Int)
getStartCoord rowNum (r:rs)
    | not (null colNum) = (rowNum,head colNum)
    | otherwise = getStartCoord (rowNum+1) rs
    where
        colNum = elemIndices 'S' r

getPart1 :: [(Char, (Int, Int))] -> Int
getPart1 path = floor (fromIntegral (1 + length path) / 2)

getPart2 :: [(Char, (Int, Int))] -> Int
getPart2 path = area + 1 - div (length path) 2
    where
        vertices = [(x,y) | (tile, (x,y)) <- path, tile == 'S' || tile == 'F' || tile == '7' || tile == 'J' || tile == 'L']
        area = div ((abs . sum) $ zipWith (\(x1, y1) (x2, y2) -> x1*y2 - x2*y1) vertices (take (length vertices) $ tail $ cycle vertices)) 2

run :: IO ()
run = do
    inp <- lines <$> readFile "src/Day10/input.txt"

    let (x,y) = getStartCoord 0 inp
    let ((sx,sy),sDir) =
            let
                n = getNextDir 'n' (inp !! (x-1) !! y)
                s = getNextDir 's' (inp !! (x+1) !! y)
                e = getNextDir 'e' (inp !! x !! (y+1))
                w = getNextDir 'w' (inp !! x !! (y-1))
            in
                if n /= '.' then ((x-1,y),n)
                else if s /= '.' then ((x+1,y),s)
                else if e /= '.' then ((x,y+1),e)
                else ((x,y-1),w)
    let path = getPath (sx, sy) inp [] sDir

    let part1 = getPart1 path
    let part2 = getPart2 path

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2