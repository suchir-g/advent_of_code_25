import Data.Char (digitToInt)
import Data.List (sortOn)

maxList :: (Ord a, Num a) => [a] -> (a, Int)
maxList xs = head $ sortOn (negate.fst) (zip xs [0..])

largestDouble :: [Int] -> Int
largestDouble xs = 10 * ae + be
    where 
        (ae, ai) = maxList (init xs)
        (be, _) = maxList $ drop (ai + 1) xs

solve :: [String] -> Int
solve xs = sum $ [largestDouble (map digitToInt x) | x <- xs]

main :: IO()
main = do 
    contents <- readFile "input.txt"
    print $ solve (lines contents)
