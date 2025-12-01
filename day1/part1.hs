solve :: String -> Int
solve xs = length $ filter (==0) $ scanl (\acc (dir:step) -> mod (acc + (-1)^(if (dir=='L') then 1 else 0) * (read step)) 100) 50 (lines xs)

main :: IO()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
