module Main where

import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.IntMap.Strict as IntMap
import Text.ParserCombinators.Parsec

type RuleType = Either [Int] String
type Rule = (Int, RuleType) 

(<:>) :: Parser String -> Parser String -> Parser String
(<:>) = liftM2 (\a b -> a ++ b)

makeRuleDict :: [Rule] -> IntMap.IntMap RuleType
makeRuleDict xs = foldr ins IntMap.empty xs
    where ins (x, y) b = IntMap.insert x y b

parseRule :: String -> Rule
parseRule str = (read $ init $ head s, rest)
    where
        s = splitOn " " str
        rest = case last s of
            "\"a\"" -> Right "a"
            "\"b\"" -> Right "b"
            _ -> Left (map read $ delete "|" $ tail s)

makeRules :: IntMap.IntMap RuleType -> Parser String
makeRules mp = helper mp 0
    where helper m a = case IntMap.lookup a m of
            Just (Right s) -> string s
            Just (Left xs) -> case length xs of
                4 -> try $ choice [(helper m $ xs !! 0) <:> (helper m $ xs !! 1), (helper m $ xs !! 2) <:> (helper m $ xs !! 3)]
                _ -> foldr1 (<:>) $ map (helper m) xs
            Nothing -> string ""

solution inp = let rule = (makeRules $ makeRuleDict $ map parseRule a) in
    map (parse rule "") b
    where (a, b) = (head inp, last inp)

main = do
    input <- fmap (splitOn "\n\n") $ readFile "inputday19"
    return (solution $ map (splitOn "\n") input)