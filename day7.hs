module Main where

import Data.List.Split

data Tree = Tree String [Tree] deriving (Show, Eq)

readLines :: FilePath -> IO [String]
readLines = fmap (splitOn "\n") . readFile

removeExtras :: Int -> String -> String
removeExtras a description = concat $ init $ drop a $ splitOn " " description

myInsert :: Tree -> Tree -> Tree
myInsert (Tree parent children) (Tree dscr bags)
    | parent == dscr = Tree dscr (children ++ bags)
    | bags == []     = Tree dscr bags
    | otherwise      = Tree dscr $ (myInsert (Tree parent children)) <$> bags

processLine :: String -> (String, [String])
processLine input = let
    outside = head parsed
    inside = last parsed
    in (removeExtras 0 outside, map (removeExtras 1) $ splitOn ", " inside)
    where parsed = splitOn " contain " input

toTree :: (String, [String]) -> Tree
toTree (parent, children) = Tree parent [Tree a [] | a <- children]

checkName :: Tree -> Tree -> Bool
checkName (Tree n1 _) (Tree n2 _) = n1 == n2

childContains :: Tree -> Tree -> Bool
childContains t (Tree _ c) = any (checkName t) c

findNode :: Tree -> [Tree] -> [Tree]
findNode t trees = filter (childContains t) trees

countBags :: Tree -> [Tree] -> [Tree] -> Int -> Int
countBags t trees seen acc 
    | elem t seen  = acc
    | otherwise    = case findNode t trees of
        [] -> acc
        xs -> sum $ fmap (\x -> countBags x trees (seen ++ xs) (acc + 1)) xs

solution :: [Tree] -> Int
solution trees = countBags (Tree "shinygold" []) trees [] 0

main :: IO Int
main = do
    input <- readLines "inputday7"
    return (solution (map (toTree . processLine) input))