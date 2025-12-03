import Data.Char (digitToInt)
import Data.List (sortOn)

maxList :: (Ord a, Num a) => [a] -> (a, Int)
maxList xs = head $ sortOn (negate.fst) (zip xs [0..])

dropBack :: Int -> [a] -> [a]
dropBack n xs = take (length xs - n) xs

largestn :: [Int] -> Int -> [Int]
largestn xs 1 = [fst $ maxList xs]
largestn xs n = ae : largestn (drop (ai + 1) xs) (n - 1)
    where (ae, ai) = maxList $ dropBack (n - 1) xs

sintToInt :: [Int] -> Int
sintToInt xs = sum $ zipWith (*) (reverse xs) (iterate (*10) 1)

solve :: [String] -> Int
solve xs = sum $ [sintToInt $ largestn (map digitToInt x) 12 | x <- xs]

main :: IO()
main = do 
    contents <- readFile "input.txt"
    print $ solve (lines contents)
