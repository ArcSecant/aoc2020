module Main where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.IntMap.Strict as IntMap

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = toBin (n `quot` 2) ++ [n `rem` 2]

toDec :: [Int] -> Int
toDec [] = 0
toDec (x : xs) = x + 2 * toDec xs

padZeroes :: [Int] -> [Int]
padZeroes xs = (take (36 - n) $ repeat 0) ++ xs
    where n = length xs

getNums :: String -> [String]
getNums = wordsBy (\x -> not $ elem x "1234567890X")

-- Part 1
maskInt :: [Char] -> Int -> Int
maskInt mask num = let zipped = zip mask $ padZeroes $ toBin num in
    toDec $ reverse $ map f zipped
    where f (a, b) = if a == 'X' then b else read $ pure a

solutionHelper :: [String] -> String -> IntMap.IntMap Int -> IntMap.IntMap Int
solutionHelper [] _ curMap = curMap
solutionHelper line curMask curMap = case getNums l of
    n:[] -> solutionHelper ls n curMap
    n:ns -> solutionHelper ls curMask $ IntMap.insert (read n) (maskInt curMask $ read $ head ns) curMap
    _ -> curMap
    where (l:ls) = line

solution :: [String] -> Int
solution ls = IntMap.foldr (+) 0 $ solutionHelper ls "" IntMap.empty

-- Part 2
interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

maskInt2 :: [Char] -> Int -> [Int]
maskInt2 mask num = map (toDec . (map digitToInt) . reverse . concat . interleave (splitOn "X" masked)) $ map (tail . splitOn "") bits
    where
        masked = map f $ zip mask $ padZeroes $ toBin num
        bits = sequence $ take (length $ filter (== 'X') masked) $ repeat "01"
        f (a, b) = case a of
            '0' -> intToDigit b
            '1' -> '1'
            c -> c

solutionHelper2 :: [String] -> String -> IntMap.IntMap Int -> IntMap.IntMap Int
solutionHelper2 [] _ curMap = curMap
solutionHelper2 line curMask curMap = let (l:ls) = line in case getNums l of
    n:[] -> solutionHelper2 ls n curMap
    n:ns -> solutionHelper2 ls curMask $ insertAll curMap (read $ head ns) $ maskInt2 curMask $ read n
    _ -> curMap
    where insertAll cur val xs = foldr (\x m -> IntMap.insert x val m) cur xs

solution2 :: [String] -> Int
solution2 ls = IntMap.foldr (+) 0 $ solutionHelper2 ls "" IntMap.empty

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday14"
    return (solution input)