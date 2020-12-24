module Main where

import Debug.Trace
import Data.List
import Data.List.Split
import qualified Data.Set as Set

parseInput :: String -> ([Int], [Int])
parseInput inp = let players = map (tail . lines) $ splitOn "\n\n" inp in
    (map read $ head players, map read $ last players)

nextRound :: ([Int], [Int]) -> ([Int], [Int])
nextRound (a, b) = let (x:xs, y:ys) = (a, b) in case x > y of
    True -> (xs ++ [x,y], ys)
    _ -> (xs, ys ++ [y,x])

playGame :: ([Int], [Int]) -> [Int]
playGame (a, []) = a
playGame ([], b) = b
playGame players = playGame $ nextRound players

solution inp = let final = playGame $ parseInput inp in
    sum $ zipWith (*) (reverse final) [1..]

-- Part 2
type Seen = Set.Set ([Int], [Int])

nextRound2 :: Seen -> ([Int], [Int]) -> ([Int], [Int])
nextRound2 seen (a, b)
    | Set.member (a, b) seen = (xs ++ [x,y], ys)
    | x <= length xs && y <= length ys = playSubGame Set.empty (take x xs, take y ys, a, b)
    | x > y = (xs ++ [x,y], ys)
    | otherwise =  (xs, ys ++ [y,x])
    where (x:xs, y:ys) = (a, b) 

playSubGame :: Seen -> ([Int], [Int], [Int], [Int]) -> ([Int], [Int])
playSubGame seen (a, b, sa, sb)
    | Set.member (a, b) seen = (xs ++ [x,y], ys)
    | null b = (xs ++ [x,y], ys)
    | null a = (xs, ys ++ [y,x])
    | otherwise = playSubGame seen' (a', b', sa, sb)
    where
        (a', b') = nextRound2 seen (a, b)
        (x:xs, y:ys) = (sa, sb)
        seen' = Set.insert (a, b) seen

playGame2 :: Seen -> ([Int], [Int]) -> ([Int], [Int])
playGame2 _ (a, []) = (a, [])
playGame2 _ ([], b) = ([], b)
playGame2 seen players = case Set.member players seen of
    True -> players
    _ -> playGame2 seen' $ nextRound2 seen players
    where seen' = Set.insert players seen

solution2 inp = let final = playGame2 Set.empty $ parseInput inp in
    case final of
        (a, []) -> sum $ zipWith (*) (reverse a) [1..]
        _ -> sum $ zipWith (*) (reverse $ snd final) [1..]

main = do
    input <- readFile "inputday22"
    print (solution2 input)