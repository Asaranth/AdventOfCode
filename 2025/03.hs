module Main where

import Data.Char (digitToInt)
import Utils (getInputData)

findMaxJoltage :: Int -> [Int] -> Int
findMaxJoltage n nums
  | n > length nums = 0
  | otherwise = digitsToInt (greedyPick n nums)
  where
    digitsToInt = foldl (\acc d -> acc * 10 + d) 0
    greedyPick 0 _ = []
    greedyPick k xs =
        let windowSize = length xs - k + 1
            window = take windowSize xs
            maxDigit = maximum window
            (_, rest) = break (== maxDigit) xs
        in maxDigit : greedyPick (k - 1) (tail rest)

part1 :: [String] -> Int
part1 banks = sum $ map (findMaxJoltage 2 . map digitToInt) banks

part2 :: [String] -> Int
part2 banks = sum $ map (findMaxJoltage 12 . map digitToInt) banks

main :: IO ()
main = do
    input <- getInputData 3
    let banks = lines input
    putStrLn $ "Part One: " ++ show (part1 banks)
    putStrLn $ "Part Two: " ++ show (part2 banks)