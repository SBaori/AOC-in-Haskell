module Day19.Solution where
import Data.List.Split (splitOn)
import qualified Data.HashMap as M
import Control.Monad (guard)
import Data.List (inits,init)

parseInp :: [String] -> ([(String,[[String]])],[[(Char,Int)]])
parseInp = (\[wf,items] -> (parseWf wf,parseItems items)) . splitOn [""]
    where
        parseWf = map ((\[a,b] -> (a, map (splitOn ":") $ splitOn "," b)) . splitOn "{" . init)
        parseItems = map (map ((\[a,b] -> (head a,read b)) . splitOn "=") . splitOn "," . init . tail)

itemRes :: [(Char,Int)] -> String -> M.Map String [[String]] -> String
itemRes item wf wfs
    | nextWf == "A" || nextWf == "R" = nextWf
    | otherwise = itemRes item nextWf wfs
    where
        nextWf = helper $ wfs M.! wf
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

f :: [Char] -> M.Map String [[String]] -> [[String]]
f "A" _ = [[]]
f "R" _ = []
f wf wfs = do
    let wfConds = wfs M.! wf
        wfConds' = map (('!':).head) $ init wfConds
    (cond:nextWf,prevConds) <- zip wfConds (inits wfConds')
    let isLast = null nextWf
        nextWf' = if isLast then cond else head nextWf
        cond' = if isLast then prevConds else cond:prevConds
    m <- f nextWf' wfs
    return (cond' ++ m)

g :: [[String]] -> Int
g = foldl (flip ((+) . M.fold (\(l,h) p -> if h>l then (h-l+1)*p else 0) 1 . h)) 0
    where
        i = M.fromList [('x',(1,4000)),('m',(1,4000)),('a',(1,4000)),('s',(1,4000))]
        h = foldl (flip h') i
        h' c m = case head c of
            '!' -> case c !! 2 of
                '>' -> M.insertWith (\(_,a) (d,b) -> (d,min a b)) (c !! 1) (0,read $ drop 3 c) m
                '<' -> M.insertWith (\(a,_) (b,d) -> (max a b,d)) (c !! 1) (read $ drop 3 c,0) m
            _ -> case c !! 1 of
                '>' -> M.insertWith (\(a,_) (b,d) -> (max a b,d)) (c !! 0) (read (drop 2 c)+1,0) m
                '<' -> M.insertWith (\(_,a) (d,b) -> (d,min a b)) (c !! 0) (0,read (drop 2 c)-1) m
                _ -> m