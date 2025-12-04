lenInt :: Int -> Int
lenInt = length . show 

getBounds :: (Int, Int) -> (Int, Int)
getBounds (lb, ub) = (nlb, nub)
    where 
        nlb = if (even . lenInt $ lb) then lb else (min ((^) 10 $ (lenInt lb)) ub)
        nub = if (even . lenInt $ ub) then ub else (max ((^) 10 $ ((lenInt $ ub) - 1)) lb)

splitFold :: String -> Char -> [String]
splitFold xs t = foldr (\x acc@(f:l) -> if (t==x) then ("" : acc) else (x:f):l) [""] xs

formatRanges :: String -> [(Int, Int)]
formatRanges xs = map (\x -> let (a:b:_) = (splitFold x '-') in (read a, read b)) $ splitFold xs ','

solve :: String -> Int
solve oStr = sum [sum [x | x <- [a..b], isRep (show x)] | (a, b) <- rngs]
    where 
        rngs = formatRanges oStr

splitlist :: [a] -> ([a], [a])
splitlist xs = splitAt (div (length xs + 1) 2) xs

strDouble :: Int -> Bool
strDouble d = a == b
    where (a, b) = (splitlist . show) d

chunk :: [a] -> Int -> [[a]]
chunk [] _ = []
chunk xs n = h : chunk t n
    where (h, t) = splitAt n xs

listEq :: Eq a => [a] -> Bool
listEq [] = True
listEq xs = all id $ zipWith (==) xs (tail xs)

isRep :: String -> Bool
isRep xs = any id [listEq $ chunk xs n | n <- [1..(length xs - 1)]]

main :: IO()
main = do 
    contents <- readFile "input.txt"
    print $ solve contents