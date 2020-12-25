module Main where

cardPubKey :: Int
cardPubKey = 13316116

doorPubKey :: Int
doorPubKey = 13651422

getLoopSize :: Int -> Int
getLoopSize key = f 1 0
    where f n ls = if n == key then ls else f ((n * 7) `mod` 20201227) (ls + 1)

doLoop :: Int -> Int -> Int
doLoop key n = f 1 0
    where f k ln = if ln == n then k else f ((k * key) `mod` 20201227) (ln + 1)

solution :: Int
solution = doLoop cardPubKey $ getLoopSize doorPubKey

main = print solution