module Main where

import Data.List
import Data.List.Split

type Constraint = (Int, Int, Int, Int)
type Ticket = [Int]

checkValid :: Int -> Constraint -> Bool
checkValid num (a,b,c,d) = (num >= a && num <= b) || (num >= c && num <= d)

checkValidAll :: [Constraint] -> Int -> Bool
checkValidAll cons num = any (checkValid num) cons

makeConstraints :: [[Int]] -> [Constraint]
makeConstraints nums = map f nums
    where f xs = (xs !! 0, xs !! 1, xs !! 2, xs !! 3)

parseInput :: [String] -> ([Constraint], Ticket, [Ticket])
parseInput inp =
    let
        constraints = makeConstraints $ map ((map read) . wordsBy (\x -> not $ elem x "1234567890")) $ splitOn "\n" cons
        yourTicket = map read $ splitOn "," $ last $ splitOn "\n" yours
        nearbyTickets = map ((map read) . splitOn ",") $ tail $ splitOn "\n" nearby
    in (constraints, yourTicket, nearbyTickets)
    where
        cons = head inp
        yours = inp !! 2
        nearby = last inp

-- Part 1
solutionHelper :: [Constraint] -> [Ticket] -> Int -> Int
solutionHelper _ [] acc = acc
solutionHelper constraints (n:ns) acc = solutionHelper constraints ns (acc + (sum $ filter (not . checkValidAll constraints) n))

solution :: [String] -> Int
solution inp = let (constraints, _, nearby) = parseInput inp in solutionHelper constraints nearby 0
        
-- Part 2
getFields :: [(Int, Constraint)] -> [Ticket] -> [Ticket] -> [Ticket]
getFields _ [] acc = acc
getFields constraints (n:ns) acc = let field = map fst $ filter (\y -> all (\x -> checkValid x (snd y)) n) constraints
    in getFields constraints ns (field:acc)

finalFields :: Int -> [Ticket] -> Ticket
finalFields l fields
    | length final == l = reverse final
    | otherwise = finalFields l [if length fld == 1 then fld else fld \\ singles | fld <- fields]
        where
            singles = foldr (++) [] $ filter (\x -> length x == 1) fields
            final = concat $ fields

solution2 :: [String] -> Int
solution2 inp = let fields = getFields (zip [0..] constraints) (transpose $ map (filter (checkValidAll constraints)) nearbyTickets) []
    in product $ zipWith f yourTicket $ finalFields (length fields) fields
    where
        (constraints, yourTicket, nearbyTickets) = parseInput inp
        f a b = if elem b [0..5] then a else 1

main :: IO Int
main = do
    input <- fmap (splitOn "\n\n") $ readFile "inputday16"
    return (solution2 input)