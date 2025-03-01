module Day19.Solution where
import Data.List.Split (splitOn)
import qualified Data.HashMap as M
import Data.List (inits, foldl')

parseInp :: [String] -> ([(String,[[String]])],[[(Char,Int)]])
parseInp = (\[wf,items] -> (parseWf wf,parseItems items)) . splitOn [""]
    where
        parseWf = map ((\[a,b] -> (a, map (splitOn ":") $ splitOn "," b)) . splitOn "{" . init)
        parseItems = map (map ((\[a,b] -> (head a,read b)) . splitOn "=") . splitOn "," . init . tail)

-- Part 1

itemRes :: [(Char,Int)] -> String -> M.Map String [[String]] -> String
itemRes item wf wfs
    | nextWf == "A" || nextWf == "R" = nextWf
    | otherwise = itemRes item nextWf wfs
    where
        nextWf = helper $ wfs M.! wf

        helper :: [[String]] -> String
        helper ((cond:wf):wfcs)
            | null wf = cond
            | (opr == '<' && val > itemVal) || (opr == '>' && val < itemVal) = head wf
            | otherwise = helper wfcs
            where
                (cat,opr,val) = (head cond, cond !! 1, read $ drop 2 cond)
                itemVal = snd $ case cat of
                    'x' -> head item
                    'm' -> item !! 1
                    'a' -> item !! 2
                    _ -> last item

getPart1 :: ([(String,[[String]])],[[(Char,Int)]]) -> Int
getPart1 (wfs, items) = foldl' (\res i -> res + if itemRes i "in" wfsMap == "A" then sum (map snd i) else 0) 0 items
    where
        wfsMap = M.fromList wfs

-- Part 2

itemAcceptConds :: [Char] -> M.Map String [[String]] -> [[String]]
itemAcceptConds "A" _ = [[]]
itemAcceptConds "R" _ = []
itemAcceptConds wf wfs = do
    let wfConds = wfs M.! wf
        wfConds' = map (('!':).head) $ init wfConds
    (cond:nextWf,prevConds) <- zip wfConds (inits wfConds')
    let isLast = null nextWf
        nextWf' = if isLast then cond else head nextWf
        cond' = if isLast then prevConds else cond:prevConds
    m <- itemAcceptConds nextWf' wfs
    return (cond' ++ m)

getPart2 :: ([(String,[[String]])],[[(Char,Int)]]) -> Int
getPart2 (wfs, _) = foldl' (flip ((+) . M.fold (\(l,h) p -> if h>l then (h-l+1)*p else 0) 1 . updateItemRanges)) 0 iac
    where
        wfsMap = M.fromList wfs

        iac = itemAcceptConds "in" wfsMap

        itemRanges = M.fromList [('x',(1,4000)),('m',(1,4000)),('a',(1,4000)),('s',(1,4000))]
        updateItemRanges = foldl' helper itemRanges

        helper :: M.Map Char (Int,Int) -> String -> M.Map Char (Int,Int)
        helper ir cond = case head cond of
            '!' -> case cond !! 2 of
                '>' -> M.insertWith (\(_,a) (d,b) -> (d,min a b)) (cond !! 1) (0,read $ drop 3 cond) ir
                '<' -> M.insertWith (\(a,_) (b,d) -> (max a b,d)) (cond !! 1) (read $ drop 3 cond,0) ir
            _ -> case cond !! 1 of
                '>' -> M.insertWith (\(a,_) (b,d) -> (max a b,d)) (cond !! 0) (read (drop 2 cond)+1,0) ir
                '<' -> M.insertWith (\(_,a) (d,b) -> (d,min a b)) (cond !! 0) (0,read (drop 2 cond)-1) ir
                _ -> ir

run :: IO ()
run = do
    pinp <- parseInp . lines <$> readFile "src/Day19/input.txt"

    let part1 = getPart1 pinp
    let part2 = getPart2 pinp

    putStrLn $ "Part1: " ++ show part1 ++ "\nPart2: " ++ show part2
