module Main where

import qualified Data.Map as Map

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeDict :: [Int] -> Map.Map Int Int
makeDict xs = Map.fromList (zip [2020 - x | x <- xs] xs)

solutionDay1:: [Int] -> Int
solutionDay1 xs = solve xs (makeDict xs)
    where
        solve nums dict = case nums of
            [] -> -1
            (y:ys) -> case Map.lookup y dict of
                Just a -> a * y
                Nothing -> solve ys dict

solutionDay1Alt :: [Int] -> Int
solutionDay1Alt xs = a * (2020 - a)
    where 
        a = head [y | x <- xs, Just y <- [Map.lookup x (makeDict xs)]]

solutionDay1Star2 :: [Int] -> Int
solutionDay1Star2 xs = a * b * c
    where
        (a, b, c) = head [(x, y, z) | x <- xs, y <- xs, z <- xs, x + y + z == 2020]

main :: IO Int
main = do
    input <- readLines "inputday1"
    return (solutionDay1Star2 $ map read input)
