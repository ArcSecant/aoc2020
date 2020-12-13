module Main where

import Data.List.Split

solution :: Int -> Int -> [Int] -> Int
solution time ini busses
    | any (\x -> (time `mod` x == 0)) busses = (time - ini) * (fst (head $ filter (\(_,y) -> y == True) $ zip busses $ map (\x -> (time `mod` x == 0)) busses))
    | otherwise = solution (time + 1) ini busses

solution2 :: [(Int, Int)] -> Int
solution2 busses = (sum $ map (\(x,y) -> findTerm x y $ m `div` y) busses) `mod` m
    where m = foldr1 (*) $ map snd busses
          findTerm x y y' = x * y' * (modInv y' y)

processInput :: [String] -> [(Int, Int)]
processInput inp = map (\(x,y) -> ((-x) `mod` read y, read y)) $ filter (\(_,y) -> y /= "x") $ zip [0..(length inp-1)] inp

modInv :: Int -> Int -> Int
modInv a m
  | 1 == g = (mkPos i)
  | otherwise = 0
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday13"
    return (solution2 $ processInput $ splitOn "," $ last input)