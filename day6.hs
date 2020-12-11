module Main where

import Data.List
import Data.List.Split

readLines :: FilePath -> IO [String]
readLines = fmap (splitOn "\n\n") . readFile

solution :: [[String]] -> Int
solution input = sum $ map solve input
    where solve = length . nub . concat

solution2 :: [[String]] -> Int
solution2 input = sum $ map solve input
    where solve = length . foldr1 intersect

main :: IO Int
main = do
    input <- readLines "inputday6"
    return (solution2 $ map (splitOn "\n") input)