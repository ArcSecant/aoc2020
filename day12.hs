module Main where

solutionHelper :: (Int, Int) -> Int -> [(Char, Int)] -> (Int, Int)
solutionHelper (x, y) _ [] = (x, y)
solutionHelper (x, y) facing (a:as) = let (dir, n) = a in
    case dir of
        'N' -> solutionHelper (x, y + n) facing as
        'S' -> solutionHelper (x, y - n) facing as
        'E' -> solutionHelper (x + n, y) facing as
        'W' -> solutionHelper (x - n, y) facing as
        'L' -> solutionHelper (x, y) (mod (facing - (n `div` 90)) 4) as
        'R' -> solutionHelper (x, y) (mod (facing + (n `div` 90)) 4) as
        'F' ->
            case facing of
                0 -> solutionHelper (x + n, y) facing as
                1 -> solutionHelper (x, y - n) facing as
                2 -> solutionHelper (x - n, y) facing as
                3 -> solutionHelper (x, y + n) facing as
                _ -> (0, 0)
        _ -> (0, 0)

solution :: [(Char, Int)] -> Int
solution input = abs a + abs b
    where (a, b) = solutionHelper (0, 0) 0 input

rotateWaypoint :: (Char, Int) -> (Int, Int) -> (Int, Int)
rotateWaypoint ('L', 90) (x, y) = (-y, x)
rotateWaypoint ('L', 180) (x, y) = (-x, -y)
rotateWaypoint ('L', 270) (x, y) = (y, -x)
rotateWaypoint ('R', 90) (x, y) = (y, -x)
rotateWaypoint ('R', 180) (x, y) = (-x, -y)
rotateWaypoint ('R', 270) (x, y) = (-y, x)
rotateWaypoint _ (x, y) = (x, y)


solutionHelper2 :: (Int, Int) -> (Int, Int) -> [(Char, Int)] -> (Int, Int)
solutionHelper2 (x, y) _ [] = (x, y)
solutionHelper2 pos waypoint (a:as) = let (dir, n) = a in
    case dir of
        'N' -> solutionHelper2 pos (w1, w2 + n) as
        'S' -> solutionHelper2 pos (w1, w2 - n) as
        'E' -> solutionHelper2 pos (w1 + n, w2) as
        'W' -> solutionHelper2 pos (w1 - n, w2) as
        'L' -> solutionHelper2 pos (rotateWaypoint a waypoint) as
        'R' -> solutionHelper2 pos (rotateWaypoint a waypoint) as
        'F' -> solutionHelper2 (x + n * w1, y + n * w2) waypoint as
        _ -> (0, 0)
    where (w1, w2) = waypoint
          (x, y) = pos

solution2 :: [(Char, Int)] -> Int
solution2 input = abs a + abs b
    where (a, b) = solutionHelper2 (0, 0) (10, 1) input

parseLine :: String -> (Char, Int)
parseLine l = (head l, read $ tail l)

main :: IO Int
main = do
    input <- fmap lines $ readFile "inputday12"
    return (solution2 $ map parseLine input)