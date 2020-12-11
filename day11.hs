module Main where

import Data.Function (fix)
import qualified Data.Set as Set

data State = Occupied | Empty | Floor deriving (Eq)
type Universe = (Set.Set (Int, Int), Set.Set (Int, Int))

convertGrid :: [[Char]] -> Universe
convertGrid input = let
    occ = getStates Occupied input
    emp = getStates Empty input
    in (Set.fromList occ, Set.fromList emp)

getStates :: State -> [[Char]] -> [(Int, Int)]
getStates state inp = concat $ map (\x -> processRow state (fst x) (snd x)) $ zip [0..length inp-1] inp

processRow :: State -> Int -> [Char] -> [(Int, Int)]
processRow Occupied r lst = [(r, i) | (a, i) <- zip lst [0..length lst-1], a == '#']
processRow Empty r lst    = [(r, i) | (a, i) <- zip lst [0..length lst-1], a == 'L']
processRow Floor r lst    = [(r, i) | (a, i) <- zip lst [0..length lst-1], a == '.']


-- Part 1
neighbours :: (Int, Int) -> [(Int, Int)] 
neighbours (x, y) = [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], (x', y') /= (x, y)]

newState :: (Int, Int) -> Universe -> State
newState pt (occ, emp)
    | Set.member pt emp = if all (\x -> not $ Set.member x occ) $ neighbours pt then Occupied else Empty
    | Set.member pt occ = if (length $ filter (== True) $ map (\x -> Set.member x occ) $ neighbours pt) > 3 then Empty else Occupied
    | otherwise         = Floor

nextState :: Universe -> Universe
nextState (occ, emp) = let
    seats = Set.union emp occ
    occ'  = Set.filter (\x -> newState x (occ, emp) == Occupied) seats
    emp'  = Set.filter (\x -> newState x (occ, emp) == Empty) seats
    in (occ', emp')

solutionHelper :: Universe -> Universe -> Int
solutionHelper begin end
    | begin == end = length occ
    | otherwise    = solutionHelper end $ nextState end
    where (occ, _) = begin

solution :: [[Char]] -> Int
solution inp = solutionHelper grid $ nextState grid  
    where grid = convertGrid inp

-- Part 2
type Universe2 = (Set.Set (Int, Int), Set.Set (Int, Int), (Int, Int))

convertGrid2 :: [[Char]] -> Universe2
convertGrid2 input = let
    occ = getStates Occupied input
    emp = getStates Empty input
    in (Set.fromList occ, Set.fromList emp, (length input, length $ head input))

neighbours2Helper :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Universe2 -> [(Int, Int)]
neighbours2Helper [] _ _ _  = []
neighbours2Helper _ [] _ _  = []
neighbours2Helper (d:ds) (dir:dirs) pt univ
    | x < 0 || x >= maxX || y < 0 || y >= maxY = neighbours2Helper ds dirs pt univ
    | Set.member (x, y) occ || Set.member (x, y) emp = (x, y):(neighbours2Helper ds dirs pt univ)
    | otherwise = neighbours2Helper ((add' dir d):ds) (dir:dirs) pt univ
        where add' (a, b) (a', b') = (a+a', b+b')
              (x, y) = add' d pt
              (occ, emp, (maxX, maxY)) = univ

neighbours2 :: (Int, Int) -> Universe2 -> [(Int, Int)]
neighbours2 pt univ = neighbours2Helper dirs dirs pt univ
    where dirs = (neighbours (0,0))

newState2 :: (Int, Int) -> Universe2 -> State
newState2 pt univ
    | Set.member pt emp = if all (\x -> not $ Set.member x occ) $ neighbours2 pt univ then Occupied else Empty
    | Set.member pt occ = if (length $ filter (== True) $ map (\x -> Set.member x occ) $ neighbours2 pt univ) > 4 then Empty else Occupied
    | otherwise         = Floor
    where (occ, emp, _) = univ

nextState2 :: Universe2 -> Universe2
nextState2 (occ, emp, var) = let
    seats = Set.union emp occ
    occ'  = Set.filter (\x -> newState2 x (occ, emp, var) == Occupied) seats
    emp'  = Set.filter (\x -> newState2 x (occ, emp, var) == Empty) seats
    in (occ', emp', var)

solutionHelper2 :: Universe2 -> Universe2 -> Int
solutionHelper2 begin end
    | begin == end = length occ
    | otherwise    = solutionHelper2 end $ nextState2 end
    where (occ, _, _) = begin

countOccupied :: Universe2 -> Int
countOccupied (occ, _, _) = length occ

recurseState :: (Universe2 -> Universe2) -> Universe2 -> Universe2
recurseState f state = if state == new then state else f new
    where new = nextState2 state

solution2 :: [[Char]] -> Int
solution2 = countOccupied . fix recurseState . convertGrid2

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday11"
    return (solution2 input)