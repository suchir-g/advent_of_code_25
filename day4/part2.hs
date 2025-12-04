import Data.Array

type Grid = Array (Int, Int) Char 

makeGrid :: Int -> [String] -> Grid
makeGrid n strs = array ((0,0), (n-1, n-1)) [((i, j), (strs !! i) !! j) | i <- [0..(n-1)], j <- [0..(n-1)]]

inBounds :: (Int, Int) -> Grid -> Bool
inBounds (x, y) gr = (x >= xlb) && (x <= xub) && (y >= ylb) && (y <= yub)  
    where ((xlb, ylb), (xub, yub)) = bounds gr
    
showAdjPositions :: (Int, Int) -> Grid -> [(Int, Int)]
showAdjPositions (x, y) gr = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], (i /= 0 || j /= 0), inBounds (x + i, y + j) gr ]

canBeTaken :: (Int, Int) -> Grid -> Bool
canBeTaken pos grid = (length nbs < 4)
    where nbs = filter (\p -> (grid ! p) == '@') $ showAdjPositions pos grid



-- solve :: Grid -> Int
-- solve grid = sum [1 | i <- [xlb..xub], j <- [ylb..yub], grid ! (i, j) == '@', canBeTaken (i, j) grid] 
--     where ((xlb, ylb), (xub, yub)) = bounds grid

main :: IO()
main = do
    contents <- readFile "input.txt"
    print $ solve (makeGrid 138 (lines contents))