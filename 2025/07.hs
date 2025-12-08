module Main where

import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Utils (getInputData)

part1 :: [String] -> Int
part1 rows =
  let h = length rows
      w = length (head rows)
      sCol = fromJust (elemIndex 'S' (head rows))
   in go 1 [sCol] 0
  where
    uniq :: [Int] -> [Int]
    uniq [] = []
    uniq (x : xs) = x : goUniq x xs
      where
        goUniq _ [] = []
        goUniq prev (y : ys)
          | y == prev = goUniq prev ys
          | otherwise = y : goUniq y ys

    go :: Int -> [Int] -> Int -> Int
    go row beams splits
      | row >= length rows = splits
      | null beams = splits
      | otherwise =
          let line = rows !! row
              (newBeamsRaw, addedSplits) = processLine line beams
              trimmed = filter (\c -> c >= 0 && c < length line) newBeamsRaw
              nextBeams = uniq (sort trimmed)
           in go (row + 1) nextBeams (splits + addedSplits)

    processLine :: String -> [Int] -> ([Int], Int)
    processLine line =
      foldr step ([], 0)
      where
        step col (accBeams, accSplits)
          | col < 0 || col >= len = (accBeams, accSplits)
          | otherwise =
              case line !! col of
                '.' -> (col : accBeams, accSplits)
                'S' -> (col : accBeams, accSplits)
                '^' -> ((col - 1) : (col + 1) : accBeams, accSplits + 1)
                _ -> (accBeams, accSplits)
        len = length line

part2 :: [String] -> Int
part2 rows =
  let h = length rows
      w = length (head rows)
      sCol = fromJust (elemIndex 'S' (head rows))
      dp0 = U.generate w (\i -> if i == sCol then 1 else 0)
   in go 0 dp0
  where
    h = length rows
    w = length (head rows)

    go :: Int -> U.Vector Int -> Int
    go row dp
      | row == h - 1 = exitCount row dp
      | otherwise =
          let line = rows !! row
              next = U.replicate w 0
              dpNext = U.ifoldl' (step line) next dp
           in go (row + 1) dpNext

    step :: String -> U.Vector Int -> Int -> Int -> U.Vector Int
    step line next col val
      | val == 0 = next
      | otherwise =
          case line !! col of
            '.' -> add next col val
            'S' -> add next col val
            '^' ->
              let left = col - 1
                  right = col + 1
                  next1 = if left >= 0 then add next left val else next
               in if right < w then add next1 right val else next1
            _ -> next

    add :: U.Vector Int -> Int -> Int -> U.Vector Int
    add vec idx val = vec U.// [(idx, vec U.! idx + val)]

    exitCount :: Int -> U.Vector Int -> Int
    exitCount row dp =
      let line = rows !! row
       in U.ifoldl'
            ( \acc col val ->
                if val == 0
                  then acc
                  else case line !! col of
                    '.' -> acc + val
                    'S' -> acc + val
                    '^' -> acc + val + val
                    _ -> acc
            )
            0
            dp

main :: IO ()
main = do
  input <- getInputData 7
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)