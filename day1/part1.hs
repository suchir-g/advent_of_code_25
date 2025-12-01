solve :: String -> Int
solve xs = length $ filter (==0) $ scanl (\acc (dir:step) -> mod (acc + (if (dir=='L') then -1 else 1) * (read step)) 100) 50 (lines xs)

main :: IO()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
