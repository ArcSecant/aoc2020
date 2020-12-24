module Main where

import Data.Maybe
import Data.List
import Data.List.Split

type Block = (Int, [String])

exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y

parseBlock :: [String] -> Block
parseBlock xs = (read $ init $ last $ splitOn " " $ head xs, orients ++ map reverse orients)
    where 
        block = tail xs
        blockT = transpose block
        orients = (head block):(head blockT):(last block):(last blockT):[]

findMatches :: [Block] -> Block -> (Int, [Int])
findMatches allBlocks (i, b) = (i, f allBlocks [])
    where
        f [] a = a
        f (x:xs) a = let (i', b') = x in
            if exists b b' && i /= i' then f xs (i':a) else f xs a

solution :: [[Char]] -> Int
solution inp = foldr1 (*) $ map fst $ filter (\(_, bs) -> length bs == 2) $ map (findMatches blocks) blocks
    where blocks = map (parseBlock . splitOn "\n") inp

-- Part 2

-- 0 -> None
-- 1 -> CCW 90 Mirror
-- 2 -> 180 Mirror
-- 3 -> CW 90
type Block2 = (Int, [(Int, Int, Int)])

exists2 :: Eq a => [a] -> [a] -> Maybe (Int, Int)
exists2 x y = case elemIndex True $ (==) <$> x <*> y of
    Just n -> Just (n `quot` 8, n `mod` 4)
    _ -> Nothing

parseBlock2 :: [String] -> Block
parseBlock2 xs = (read $ init $ last $ splitOn " " $ head xs, orients)
    where 
        block = tail xs
        blockT = transpose block
        orients = (head block):(last blockT):(reverse $ last block):(reverse $ head blockT):[]

getBlockPositions :: [Block] -> Block -> Block2
getBlockPositions allBlocks (i, b) = f allBlocks (i, [])
    where
        f [] a = a
        f (x:xs) (y, a) = let (i', b') = x in
            case exists2 b b' of
                Just (r, c) -> f xs (y, (i', r, c):a)
                Nothing -> f xs (y,a)

-- Start with any corner

solution2 :: [[Char]] -> Int
solution2 inp = length $ filter (\(_, bs) -> length bs == 4) $ map (findMatches blocks) blocks
    where blocks = map (parseBlock . splitOn "\n") inp

main = do
    input <- fmap (splitOn "\n\n") $ readFile "inputtest"
    return (solution2 input)