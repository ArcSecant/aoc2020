module Main where

import Data.List

diff :: [Int] -> [Int] -> [Int]
diff a b = map (\(p, q) -> p - q) $ zip a b

solution :: [Int] -> Int
solution xs = let
        sorted = sort xs
        diffs = diff (sorted ++ [(last sorted) + 3]) $ [0] ++ sorted
    in (length $ filter (== 1) diffs) * (length $ filter (== 3) diffs)
        
solutionHelper :: (Int, [Int]) -> ([Int], [Int]) -> Int -> Int
solutionHelper (upTo, joltLst) queue cur
    | cur == upTo      = sum' queue
    | elem cur joltLst = solutionHelper (upTo, joltLst) (enq queue $ sum' queue) (cur + 1)
    | otherwise        = solutionHelper (upTo, joltLst) (enq queue 0) (cur + 1)
    where
        enq (l, []) x = enq ([], reverse l) x
        enq (l, (_:r)) x = (x:l, r)
        sum' (l,r) = (sum l + sum r)

solutionHelper2 :: (Int, [Int]) -> [Int] -> Int -> Int
solutionHelper2 (upTo, joltLst) queue cur
    | cur == upTo      = sum queue
    | elem cur joltLst = solutionHelper2 (upTo, joltLst) (enq queue $ sum queue) (cur + 1)
    | otherwise        = solutionHelper2 (upTo, joltLst) (enq queue 0) (cur + 1)
    where
        enq q x = tail q ++ [x]

solution2 :: [Int] -> Int
solution2 xs = solutionHelper (maximum xs, xs) ([], firstThree) $ last firstThree
    where firstThree = [1, minimum xs  + 1, (+) 1 $ sum $ take 2 $ sort xs]

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday10"
    return (solution2 $ map read input)