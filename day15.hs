module Main where

import qualified Data.Map as Map

sol :: Int -> Map.Map Int Int -> Int -> Int
sol prevNum numMap curTurn = case curTurn of
    30000001 -> prevNum
    _ -> case numMap Map.!? prevNum of 
        Nothing -> sol 0 (Map.insert prevNum (curTurn - 1) numMap) $ curTurn + 1
        Just a  -> sol (curTurn - a - 1) (Map.insert prevNum (curTurn - 1) numMap) $ curTurn + 1

inp :: Map.Map Int Int
inp = Map.fromList $ zip [20,9,11,0,1] [1..5]

main :: IO ()
main = print $ show $ sol 2 inp 7