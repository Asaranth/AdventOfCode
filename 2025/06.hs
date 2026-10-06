-- | Day 06: Column Operatinator
--
-- Evaluates arithmetic worksheet expressions formatted in vertical columns via transpose reductions and streaming digit parsing.
module Main where

import Data.Char (isDigit)
import Data.List (transpose)
import Utils (getInputData)

-- | Solves Part One: computes column totals by applying bottom-row operators to vertical operands.
part1 :: [[String]] -> Int
part1 columns = sum $ map solve columns
  where
    solve col = applyOp (last col) (map read (init col))
    applyOp "*" = product
    applyOp "+" = sum
    applyOp _ = const 0

-- | Solves Part Two: parses columns right-to-left into multi-digit numbers and reduces each group with its operator.
part2 :: [String] -> Int
part2 linesInput =
  let maxLength = maximum (map length linesInput)
      paddedLines = map (padRight maxLength) linesInput
      columns = transpose paddedLines
      reversedColumns = reverse columns
   in solveP2 reversedColumns [] 0
  where
    padRight n s = s ++ replicate (n - length s) ' '
    solveP2 [] _ total = total
    solveP2 (col : cols) pending total =
      let op = last col
          digits = init col
          numStr = filter isDigit digits
          newPending = if null numStr then pending else (read numStr : pending)
       in if op == '+' || op == '*'
            then
              let res = applyCharOp op newPending
               in solveP2 cols [] (total + res)
            else
              solveP2 cols newPending total
    applyCharOp '+' = sum
    applyCharOp '*' = product
    applyCharOp _ = const 0

main :: IO ()
main = do
  input <- getInputData 6
  let ls = lines input
  let parsed1 = transpose $ map words ls
  putStrLn $ "Part One: " ++ show (part1 parsed1)
  putStrLn $ "Part Two: " ++ show (part2 ls)