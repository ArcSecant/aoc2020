module Main where

import Data.List
import qualified Data.Set as Set

type Point = [Int]
type Cubes = Set.Set Point

neighbours :: Point -> [Point]
neighbours p = delete p $ sequence $ map (\x -> [x-1..x+1]) p

processRow :: Int -> Int -> [Char] -> [Point]
processRow d r lst = [[r, i] ++ take (d - 2) [0..] | (a, i) <- zip lst [0..], a == '#']

getStates :: Int -> [[Char]] -> [Point]
getStates d inp = concat $ zipWith f [0..] inp
    where f a b = processRow d a b

convertGrid :: Int -> [[Char]] -> Cubes
convertGrid d input = Set.fromList alive
    where 
        alive = getStates d input

getNewState :: Cubes -> Point -> Bool
getNewState alive p =
    if lenAlive == 3 then True else
        if Set.member p alive && lenAlive == 2 then True else False
    where
        lenAlive = length [n | n <- neighbours p, Set.member n alive]

nextStateFaster :: Cubes -> Int -> Int -> Cubes
nextStateFaster allCubes n d = let board = Set.fromList $ sequence $ take d $ repeat [-2-n..9+n] in
    fst $ Set.partition (getNewState allCubes) board

solution :: [[Char]] -> Int
solution cubes = Set.size $ f (convertGrid 3 cubes) (0 :: Int)
    where
        f cs 6 = cs
        f cs c = f (nextStateFaster cs c 3) (c+1)

solution2 :: [[Char]] -> Int
solution2 cubes = Set.size $ f (convertGrid 4 cubes) (0 :: Int)
    where
        f cs 6 = cs
        f cs c = f (nextStateFaster cs c 4) (c+1)

main :: IO ()
main = do
    input <- fmap lines $ readFile "inputday17"
    print $ show (solution2 input)