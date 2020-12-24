module Main where

import Data.List
import qualified Data.HashMap.Strict as HashMap

type HexPoint = (Int, Int, Int)

hexAdd :: HexPoint -> HexPoint -> HexPoint
hexAdd (a, b, c) (x, y, z) = (a + x, b + y, c + z)

getPoint :: String -> HexPoint
getPoint xs = helper xs (0, 0, 0)
    where
        helper [] a = a
        helper str a
            | isPrefixOf "e"  str = helper (drop 1 str) $ hexAdd a (1, -1, 0)
            | isPrefixOf "se" str = helper (drop 2 str) $ hexAdd a (0, -1, 1)
            | isPrefixOf "sw" str = helper (drop 2 str) $ hexAdd a (-1, 0, 1)
            | isPrefixOf "w"  str = helper (drop 1 str) $ hexAdd a (-1, 1, 0)
            | isPrefixOf "nw" str = helper (drop 2 str) $ hexAdd a (0, 1, -1)
            | isPrefixOf "ne" str = helper (drop 2 str) $ hexAdd a (1, 0, -1)
            | otherwise =  (0, 0, 0)

makeMap :: [String] -> Floor -> Floor
makeMap [] m = m
makeMap (x:xs) m = makeMap xs $ g (getPoint x) m
    where
        g p m' = case HashMap.lookup p m' of
            Just v -> HashMap.insert p (v + 1) m'
            _ -> HashMap.insert p 1 m'

solution :: [String] -> Int
solution str = HashMap.size $ HashMap.filter odd $ makeMap str HashMap.empty

-- Part 2
type Floor = HashMap.HashMap HexPoint Int

neighbours :: HexPoint -> [HexPoint]
neighbours p = map (hexAdd p) [(a, b, c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], a + b + c == 0, (a, b, c) /= (0, 0, 0)]

nextSingleState :: Floor -> HexPoint -> Int
nextSingleState fl p = case HashMap.lookup p fl of
    Just v -> case odd v of
        True -> if l == 0 || l > 2 then 1 else 0
        _ -> if l == 2 then 1 else 0
    Nothing -> if l == 2 then 1 else 0
    where
        l = length $ filter odd $ map f $ neighbours p
        f x = case HashMap.lookup x fl of
            Just v -> v
            _ -> 0

nextState :: Floor -> Floor
nextState fl = HashMap.mapWithKey f fl
    where f k v = v + nextSingleState fl k

initFloor :: Floor
initFloor = HashMap.fromList $ zip [(a, b, c) | a <- [-100..100], b <- [-100..100], c <- [-100..100], a + b + c == 0] [0,0..]

solution2 :: [String] -> Int
solution2 str = HashMap.size $ HashMap.filter odd $ f 100 $ makeMap str initFloor
    where f n m
            | n == 0 = m
            | otherwise = f (n-1) $ nextState m

main = do
    input <- fmap lines $ readFile "inputday24"
    print (solution2 input)