module Main where

import Prelude hiding ((++), map, head, tail, drop, splitAt)
import Data.Vector

type VInt = Vector Int

nextMove :: VInt -> VInt
nextMove xs = snoc (tail new) $ head new
    where new = rearrange $ pickupCups xs

rearrange :: (VInt, VInt) -> VInt
rearrange (cups, pickup) = left ++ pickup ++ right
    where
        (left, right) = destCup (mod (head cups-1) 9) cups

destCup :: Int -> VInt -> (VInt, VInt)
destCup n xs = case elemIndex n xs of
    Just a -> splitAt (a+1) xs
    _ -> destCup (mod (n-1) 9) xs

pickupCups :: VInt -> (VInt, VInt)
pickupCups xs = let nextThree = fromList [xs ! 1, xs ! 2, xs ! 3] in
    (cons (head xs) (drop 4 xs), nextThree)

input :: VInt
input = map (pred . read . pure) $ fromList "389125467"

input2 :: VInt
input2 = (map (pred . read . pure) $ fromList "389125467") ++ fromList [9..999999]

solution inp = f inp 0
    where f xs a = if a == 100 then map succ xs else f (nextMove xs) $ a + 1

main = print $ solution input