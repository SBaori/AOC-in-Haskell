module Day8.Solution where
import Data.List (sortBy, sort, find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Utils (printSolution)

dist :: Num a => ([a], [a]) -> a
dist (p1, p2) = sum $ zipWith (\c1 c2 -> (c1-c2)*(c1-c2)) p1 p2

findparentAndCompress :: Int -> M.Map Int Int-> M.Map Int Int
findparentAndCompress id circuitMap
    | M.findWithDefault id id circuitMap == id = circuitMap
    | otherwise = M.insert id (M.findWithDefault parentId parentId circuitMap') circuitMap'
    where
        circuitMap' = findparentAndCompress (circuitMap M.! id) circuitMap
        parentId = circuitMap M.! id

createCircuit' :: (Int, Int) -> M.Map Int Int -> M.Map Int Int
createCircuit' (id1, id2) circuitMap
    | parentId1 == parentId2 = circuitMap'
    | otherwise = M.insert parentId2 parentId1 circuitMap'
    where
        circuitMap' = findparentAndCompress id2 $ findparentAndCompress id1 circuitMap
        parentId1 = M.findWithDefault id1 id1 circuitMap'
        parentId2 = M.findWithDefault id2 id2 circuitMap'

getCircuitsAndInsertedPoints :: [[Int]] -> [(([Int], [Int]), M.Map Int Int)]
getCircuitsAndInsertedPoints points = zip orderedPoints $ tail circuits
    where
        orderedPoints = sortBy (\p1 p2 ->
                                    if dist p1 < dist p2 then
                                        LT
                                    else
                                        GT
                                ) [(x, y) | x <- points, y <- points, x < y]

        pointIdMap = M.fromList $ zip points [0..]

        circuits = M.empty : ((\((p1, p2), cm)
                                    -> createCircuit' (pointIdMap M.! p1, pointIdMap M.! p2) cm
                                ) <$> zip orderedPoints circuits
                            )

run :: IO ()
run = do
    linp <- lines <$> readFile "src/Day8/input.txt"

    let points = map (map (read :: String -> Int) . splitOn ",") linp
    let pointCnt = length points

    let circuitsAndInsertedPoint = getCircuitsAndInsertedPoints points

    let (_, circuitMap) = circuitsAndInsertedPoint !! 1000

    let part1 = (product . take 3 . (reverse . sort))
                $ map ((+1) . snd)
                $ (M.toList . M.fromListWith (+))
                $ map ((, 1) . (\(p, _)
                            -> M.findWithDefault p p $ findparentAndCompress p circuitMap)
                        ) (M.toList circuitMap)

    let part2 = do
            ((x1:_, x2:_), _) <- find (\((p1, p2), c) -> M.size c == pointCnt-1) circuitsAndInsertedPoint
            pure (x1*x2)

    printSolution(show part1, show $ fromMaybe 0 part2)