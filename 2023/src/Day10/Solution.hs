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

getPath :: (Int, Int) -> [[Char]] -> [(Int,Int)] -> Char -> [(Int,Int)]
getPath (x,y) tiles loop dir
    | tiles !! x !! y == 'S' = (x,y):loop
    | dir == 'n' = getPath (x-1,y) tiles ((x,y):loop) (getNextDir dir (tiles !! (x-1) !! y))
    | dir == 's' = getPath (x+1,y) tiles ((x,y):loop) (getNextDir dir (tiles !! (x+1) !! y))
    | dir == 'e' = getPath (x,y+1) tiles ((x,y):loop) (getNextDir dir (tiles !! x !! (y+1)))
    | otherwise = getPath (x,y-1) tiles ((x,y):loop) (getNextDir dir (tiles !! x !! (y-1)))

getStartCoord :: Int -> [String] -> (Int,Int)
getStartCoord rowNum (r:rs)
    | not (null colNum) = (rowNum,head colNum)
    | otherwise = getStartCoord (rowNum+1) rs
    where
        colNum = elemIndices 'S' r

getPart1 :: [String] -> Int
getPart1 inp = floor (fromIntegral (1 + length (getPath (sx,sy) inp [] sDir)) / 2)
    where
        (x,y) = getStartCoord 0 inp
        ((sx,sy),sDir) =
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

g _ _ count [] _ _ = count
g t1 t2 count ((x,y):cs) path grid
    | contains && tile == '|' = g t1 t2 (count+1) cs path grid
    | contains && tile == 'L' = g '7' t2 count cs path grid
    | contains && tile == '7' && t1 == '7' = g '.' t2 (count+1) cs path grid
    | contains && tile == 'F' = g t2 'J' count cs path grid
    | contains && tile == 'J' && t2 == 'J' = g t1 '.' (count+1) cs path grid
    | otherwise = g t1 t2 count cs path grid
    where
        contains = not $ null $ elemIndices (x,y) path
        tile = grid !! x !! y

h [] count _ _ = count
h (r:rs) count path grid = h rs (f r 0 + count) path grid
    where
        f [] c = c
        f (p:ps) c
            | isNothing (elemIndex p path) = f ps (c + mod (g '.' '.' 0 ps path grid) 2)
            | otherwise = f ps c