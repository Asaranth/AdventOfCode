-- | Day 08: Playground
--
-- Connects 3D spatial points using disjoint set union (DSU) to evaluate component sizes and spanning edge properties.
module Main where

import Control.Monad (when)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (group, sort, sortOn)
import Utils (getInputData)

-- | 3D coordinate point represented by (x, y, z) values.
type Point = (Int, Int, Int)

-- | Disjoint Set Union structure stored in an unboxed ST array.
type DSU s = STUArray s Int Int

-- | Parses a list of comma-separated coordinate strings into 3D points.
parse :: [String] -> [Point]
parse = map parseLine
  where
    parseLine s =
      let [x, y, z] = map read (words (map (\c -> if c == ',' then ' ' else c) s))
       in (x, y, z)

-- | Computes the squared Euclidean distance between two 3D points.
dist2 :: Point -> Point -> Int
dist2 (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

-- | Generates all pairwise index combinations and their squared distance.
allPairs :: [Point] -> [((Int, Int), Int)]
allPairs pts = [((i, j), dist2 (pts !! i) (pts !! j)) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1]]
  where
    n = length pts

-- | Generates and sorts all pairwise point distances in ascending order.
sortedPairs :: [Point] -> [((Int, Int), Int)]
sortedPairs pts = sortOn snd (allPairs pts)

-- | Initialises a new DSU structure where each element is its own parent.
newDSU :: Int -> ST s (DSU s)
newDSU n = newListArray (0, n - 1) [0 .. n - 1]

-- | Finds the representative root of a set with path compression.
findDSU :: DSU s -> Int -> ST s Int
findDSU parent x = do
  p <- readArray parent x
  if p == x
    then return x
    else do
      root <- findDSU parent p
      writeArray parent x root
      return root

-- | Merges two sets in the DSU structure by linking their roots.
unionDSU :: DSU s -> Int -> Int -> ST s ()
unionDSU parent a b = do
  ra <- findDSU parent a
  rb <- findDSU parent b
  when (ra /= rb) (writeArray parent ra rb)

-- | Solves Part One: unions the 1000 shortest edges and returns the product of the three largest component sizes.
part1 :: [String] -> Int
part1 ls =
  let pts = parse ls
      edges = take 1000 (sortedPairs pts)
      n = length pts
      sizes = runST $ do
        dsu <- newDSU n
        mapM_ (\((i, j), _) -> unionDSU dsu i j) edges
        roots <- mapM (findDSU dsu) [0 .. n - 1]
        let grouped = map length . group . sort $ roots
        return (reverse (sort grouped))
      (a : b : c : _) = sizes
   in a * b * c

-- | Solves Part Two: incrementally unions nearest edges until all points form a single component, returning the x-coordinate product of the final edge.
part2 :: [String] -> Int
part2 ls =
  let pts = parse ls
      edges = sortedPairs pts
      n = length pts
   in runST $ do
        dsu <- newDSU n
        let go comps (((i, j), _) : es)
              | comps == 1 = error "Already unified before processing all edges"
              | otherwise = do
                  ri <- findDSU dsu i
                  rj <- findDSU dsu j
                  if ri /= rj
                    then do
                      unionDSU dsu i j
                      let comps' = comps - 1
                      if comps' == 1
                        then do
                          let (xi, _, _) = pts !! i
                          let (xj, _, _) = pts !! j
                          return (xi * xj)
                        else go comps' es
                    else go comps es
            go _ [] = error "Ran out of edges before fully connecting everything"
        go n edges

main :: IO ()
main = do
  input <- getInputData 8
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)
