module Main where

import GHC.Exts
import Data.List
import Data.List.Split
import qualified Data.Set as Set

type Ingredients = Set.Set String
type Allergens = Set.Set String
type Food = (Ingredients, Allergens)

exists :: Eq a => [a] -> [a] -> Bool
exists x y = any id $ (==) <$> x <*> y

parseLine :: String -> Food
parseLine str = let s = map (filter (\x -> not $ elem x ",()")) $ splitOn " (contains " str in
    (Set.fromList $ words $ head s, Set.fromList $ words $ last s)

getAllergens :: [Food] -> Allergens
getAllergens fd = foldr1 Set.union $ map snd fd

potentialAllergens :: [Food] -> String -> Ingredients
potentialAllergens allFood fd = foldr1 Set.intersection [i | (i, a) <- allFood, Set.member fd a]

solution :: [String] -> Int
solution inp = let allergens = foldr1 Set.union $ Set.map (potentialAllergens allFood) $ getAllergens allFood in
    sum $ map Set.size $ [i Set.\\ allergens | (i, _) <- allFood]
    where allFood = map parseLine inp

-- Part 2
translate :: [Food] -> String -> (String, Ingredients)
translate allFood fd = (fd, foldr1 Set.intersection [i | (i, a) <- allFood, Set.member fd a])

processAllergens :: [(String, Ingredients)] -> [(String, String)]
processAllergens potentials
    | (sum $ map (Set.size . snd) potentials) == length potentials = [(a, Set.elemAt 0 i) | (a, i) <- potentials]
    | otherwise = processAllergens [if Set.size i == 1 then (a, i) else (a, i Set.\\ singles) | (a, i) <- potentials]
        where
            singles = foldr Set.union Set.empty [i | (a, i) <- potentials, Set.size i == 1]

solution2 :: [String] -> String
solution2 inp = let dList = map snd $ sortWith fst $ processAllergens $ map (translate allFood) $ Set.toList $ getAllergens allFood in
    intercalate "," dList
    where allFood = map parseLine inp

main = do
    input <- fmap lines $ readFile "inputday21"
    return (solution2 input)