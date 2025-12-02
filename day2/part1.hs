lenInt :: Int -> Int
lenInt = length . show 

getBounds :: (Int, Int) -> (Int, Int)
getBounds (lb, ub) = (nlb, nub)
    where 
        nlb = if (even . lenInt $ lb) then lb else (min ((^) 10 $ (lenInt lb)) ub)
        nub = if (even . lenInt $ ub) then ub else (max ((^) 10 $ ((lenInt $ ub) - 1)) lb)

-- do split with a fold later

split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) t = if (t==x) then ("" : sxs) else (x:f):l
    where sxs@(f:l) = split xs t

splitFold :: String -> Char -> String

formatRanges :: String -> [(Int, Int)]
formatRanges xs = map (\x -> let (a:b:_) = (split x '-') in (read a, read b)) $ split xs ','

solve :: String -> Int
solve oStr = sum [sum [x | x <- [a..b], isRep x] | (a, b) <- rngs]
    where 
        rngs = map getBounds $ formatRanges oStr

splitlist :: [a] -> ([a], [a])
splitlist xs = splitAt (div (length xs + 1) 2) xs

isRep :: Int -> Bool
isRep d = a == b
    where (a, b) = (splitlist . show) d

main :: IO()
main = do 
    contents <- readFile "input.txt"
    print $ solve contents