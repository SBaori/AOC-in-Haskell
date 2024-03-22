module Day15.Solution where

import Data.Char (isDigit, isLetter)
import Data.List (elemIndex, findIndex, groupBy)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)

parseInp :: [String] -> [String]
parseInp = head . map (splitOn ",")

hash :: String -> Int
hash = foldl (\currVal char -> mod (17 * (currVal + fromEnum char)) 256) 0

getPart1 :: [String] -> Int
getPart1 = sum . map hash . parseInp

getPart2 :: [String] -> Int
getPart2 = sum . map boxPower . hashmap [[] | _ <- [0 .. 255]] . parseInp
  where
    boxPower (bNum, lenses) = foldl (\acc (pos, (_, f)) -> acc + (bNum + 1) * pos * f) 0 (zip [loc, loc - 1 .. 1] lenses)
      where
        loc = length lenses
    
    hashmap boxes =
        foldl
            ( \b opr ->
                let
                    (newLabel : action : rest) = groupBy (\x y -> (isLetter x && isLetter y) || (isDigit x && isDigit y)) opr
                    newHash = hash newLabel
                 in
                    map
                        ( \items@(bNum, bItems) ->
                            if bNum == newHash
                                then (newHash, insertLens bItems (newLabel, action, rest))
                                else items
                        )
                        b
            )
            (zip [0 ..] boxes)
      where
        insertLens bItems (newLabel, action, rest)
            | action == "=" =
                if isNothing (findIndex ((== newLabel) . fst) bItems)
                    then (newLabel, newFl) : bItems
                    else map (\lens@(label, fl) -> if label == newLabel then (newLabel, newFl) else lens) bItems
            | otherwise = filter ((/= newLabel) . fst) bItems
          where
            newFl = (read :: String -> Int) $ head rest