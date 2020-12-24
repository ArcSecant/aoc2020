module Main where

import Text.ParserCombinators.Parsec

num :: Parser Int
num = do
    n <- many1 digit
    return $ read n

parens :: Parser Int
parens = between (char '(') (char ')') expr

expr :: Parser Int
expr = add' <|> term

expr2 :: Parser Int
expr2 = add <|> term

term :: Parser Int
term = mul <|> factor

factor :: Parser Int
factor = parens <|> num

add :: Parser Int
add = try $ do
    left <- term
    _ <- char '*'
    right <- expr
    return (left * right)

add' :: Parser Int
add' = try $ do
    left <- term
    op <- (char '+') <|> (char '*')
    right <- expr
    return (case op of
        '+' -> left + right
        _ -> left * right)

mul :: Parser Int
mul = try $ do
    left <- factor
    _ <- (char '+')
    right <- term
    return (left + right)

removeWhitespace :: String -> String
removeWhitespace = filter (/=' ')

invertBrackets :: String -> String
invertBrackets = map f
    where
        f ')' = '('
        f '(' = ')'
        f a = a

solution :: [String] -> Int
solution xs = foldr1 (+) $ map (f . invertBrackets . reverse . removeWhitespace) xs
    where f s = case parse expr "" s of
            Left _ -> 0
            Right a -> a


solution2 :: [String] -> Int
solution2 xs = foldr1 (+) $ map (f . removeWhitespace) xs
    where f s = case parse expr2 "" s of
            Left _ -> 0
            Right a -> a

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday18"
    return (solution input)