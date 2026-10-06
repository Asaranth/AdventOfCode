-- | Day 01: Secret Entrance
--
-- Simulates dial movements along a circular track using modular arithmetic and integer-division wrap-around analysis.
module Main where

import Utils (getInputData)

-- | Total number of discrete positions on the circular dial track.
trackSize :: Int
trackSize = 100

-- | Parses a raw movement instruction string into a direction character and step magnitude.
parseInstr :: String -> (Char, Int)
parseInstr (d : ds) = (d, read ds)
parseInstr _ = error "Invalid instruction"

-- | Iterates through instructions starting from an initial position, accumulating event hits.
iterateInstr :: (Int -> Char -> Int -> (Int, Int)) -> Int -> [String] -> Int
iterateInstr step start = go start 0
  where
    go _ count [] = count
    go pos count (x : xs) =
      let (dir, val) = parseInstr x
          (pos', hits) = step pos dir val
       in go pos' (count + hits) xs

-- | Solves Part One: counts how many times the dial stops exactly at position 0.
part1 :: [String] -> Int
part1 = iterateInstr step1 50
  where
    step1 pos dir val =
      let newPos = case dir of
            'L' -> (pos - val) `mod` trackSize
            'R' -> (pos + val) `mod` trackSize
            _ -> pos
          hits = if newPos == 0 then 1 else 0
       in (newPos, hits)

-- | Solves Part Two: counts the total number of times the dial crosses or lands on position 0.
part2 :: [String] -> Int
part2 = iterateInstr step2 50
  where
    step2 pos 'L' val =
      let end = (pos - val) `mod` trackSize
          hits = (pos - 1) `div` trackSize - (pos - val - 1) `div` trackSize
       in (end, hits)
    step2 pos 'R' val =
      let end = (pos + val) `mod` trackSize
          hits = (pos + val) `div` trackSize - pos `div` trackSize
       in (end, hits)
    step2 pos _ _ = (pos, 0)

main :: IO ()
main = do
  input <- getInputData 1
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)