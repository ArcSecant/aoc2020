module Main where

import Data.List
import Data.List.Split

readLines :: FilePath -> IO [String]
readLines = fmap (splitOn "\n") . readFile

toDecimal :: [Int] -> Int
toDecimal [] = 0
toDecimal (x : xs) = x + 2 * toDecimal xs

solution :: [String] -> Int
solution bpass = maximum $ map (\xs -> (convert $ take 7 xs) * 8 + (convert $ drop 7 xs)) bpass
    where convert = toDecimal . reverse . map (\x -> if x `elem` "FL" then 0 else 1)

solution2 :: [String] -> Int
solution2 bpass = let seats = map (\xs -> (convert $ take 7 xs) * 8 + (convert $ drop 7 xs)) bpass
    in head $ [128..(127*8)] \\ seats
    where convert = toDecimal . reverse . map (\x -> if x `elem` "FL" then 0 else 1)

main :: IO Int
main = do
    input <- readLines "inputday5"
    return (solution2 $ input)