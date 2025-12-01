solve :: String -> Int
solve xs = snd $ foldl cum (50, 0) (lines xs)
  where
    cum (pos, nzs) (dir:step) =
      let (turns, rotation) = read step `divMod` 100
          extra = if dir == 'R' then if pos + rotation >= 100 then 1 else 0
                  else if pos > 0 && pos - rotation <= 0 then 1 else 0
          newPos = (pos + (if dir == 'L' then -rotation else rotation)) `mod` 100
      in (newPos, nzs + turns + extra)

main :: IO()
main = do
    contents <- readFile "input.txt"
    print $ solve contents
