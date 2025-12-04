import Data.Array

type Grid = Array (Int, Int) Char 

tempGrid :: Grid
tempGrid = makeGrid 3 ["@@.", "@@@", ".@."]

makeGrid :: Int -> [String] -> Grid
makeGrid n strs = array ((0,0), (n-1, n-1)) [((i, j), (strs !! i) !! j) | i <- [0..(n-1)], j <- [0..(n-1)]]

inBounds :: (Int, Int) -> Grid -> Bool
inBounds (x, y) gr = (x >= xlb) && (x <= xub) && (y >= ylb) && (y <= yub)  
    where ((xlb, ylb), (xub, yub)) = bounds gr
    
showAdjPositions :: (Int, Int) -> Grid -> [(Int, Int)]
showAdjPositions (x, y) gr = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], (i /= 0 || j /= 0), inBounds (x + i, y + j) gr ]

canBeTaken :: (Int, Int) -> Grid -> Bool
canBeTaken pos grid = (length nbs < 4) && grid ! pos == '@'
    where nbs = filter (\p -> (grid ! p) == '@') $ showAdjPositions pos grid

goDeep :: ((Int, Grid) -> (Int, Grid)) -> Grid -> Int
goDeep remover startGrid = fst $ goDeeper (0, startGrid) 
    where 
        goDeeper :: (Int, Grid) -> (Int, Grid)
        goDeeper cState@(rnScore, rnGrid) = (restScore, restGrid) 
            where 
                nextState@(nextScore, nextGrid) = remover cState
                restState@(restScore, restGrid) = if (rnGrid == nextGrid) then nextState else goDeeper nextState


remFromGrid :: (Int, Grid) -> (Int, Grid)
remFromGrid (score, grid) = (score + length popped, newGrid)
    where
        popped = [(i,j) | i <- [xlb..xub], j <- [ylb..yub], canBeTaken (i,j) grid]
        newGrid = grid//[((i, j), if  (canBeTaken (i,j) grid) then '.' else grid ! (i,j)) | i <- [xlb..xub], j <- [ylb..yub]]
        ((xlb, ylb), (xub, yub)) = bounds grid


solve :: Grid -> Int
solve grid = goDeep remFromGrid grid

main :: IO()
main = do
    contents <- readFile "input.txt"
    print $ solve (makeGrid 138 (lines contents))