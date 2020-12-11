module Main where

import Control.Monad
import Data.List

windows :: Int -> [Int] -> [[Int]]
windows n xs = take (length xs - n + 1) $ transpose $ take n $ tails xs

solution :: [Int] -> Int
solution nums = last $ head $ filter (\x -> findAns x) $ windows 26 nums
    where findAns xs = null [x + y | (x, y) <- liftM2 (,) (init xs) (init xs), x /= y, x + y == last xs]

target :: Int
target = 29221323

solution2 :: [Int] -> Int
solution2 nums = let
    helper n xs = case filter (\x -> sum x == target) $ windows n xs of
        [] -> helper (n + 1) xs 
        x  -> head x
    ans = helper 2 nums
    in minimum ans + maximum ans

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday9"
    return (solution2 $ map read input)