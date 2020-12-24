module Main where

import Data.List
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set

type HexPoint = (Int, Int, Int)
type Floor = HashMap.HashMap HexPoint Int

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
type Tiles = Set.Set HexPoint
type Floor2 = (Tiles, Tiles)
data State = White | Black deriving (Eq)

neighbours :: HexPoint -> [HexPoint]
neighbours p = map (hexAdd p) [(a, b, c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], a + b + c == 0, (a, b, c) /= (0, 0, 0)]

nextSingleState :: Floor2 -> HexPoint -> State
nextSingleState (_, bt) p = case Set.member p bt of
    True -> if l == 0 || l > 2 then White else Black
    _ -> if l == 2 then Black else White
    where
        l = length $ filter (\x -> Set.member x bt) $ neighbours p

nextState :: Floor2 -> Floor2
nextState fl = let
    (wt1, wt2) = Set.partition (\x -> nextSingleState fl x == White) wt
    (bt1, bt2) = Set.partition (\x -> nextSingleState fl x == White) bt
    in (Set.union wt1 bt1, Set.union wt2 bt2)
    where (wt, bt) = fl

initFloor :: Floor2
initFloor = (Set.fromList [(a, b, c) | a <- [-75..75], b <- [-75..75], c <- [-75..75], a + b + c == 0], Set.empty)

makeMap2 :: [String] -> Floor2 -> Floor2
makeMap2 [] m = m
makeMap2 (x:xs) (wt, bt) = makeMap2 xs $ g (getPoint x)
    where
        g p = case Set.member p wt of
            True -> (Set.delete p wt, Set.insert p bt)
            _ -> (Set.insert p wt, Set.delete p bt)

solution2 :: [String] -> Int
solution2 str = Set.size $ snd $ f 100 $ makeMap2 str initFloor
    where f n m
            | n == 0 = m
            | otherwise = f (n-1) $ nextState m

main = do
    input <- fmap lines $ readFile "inputday24"
    print (solution2 input)