module Main where

import Data.Bits (finiteBitSize, popCount, shiftL, xor, (.&.), (.|.))
import Data.List (foldl', isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (getInputData)

data Machine = Machine {targetVal :: Int, targetStr :: String, buttons :: [Int], buttonsMap :: [M.Map Int Int], joltages :: [Int]} deriving (Show, Eq)

lightsToMask :: String -> Int
lightsToMask s = foldl' (\acc c -> acc * 2 + if c == '#' then 1 else 0) 0 . reverse $ filter (`elem` "#.") s

buttonToMask :: String -> Int
buttonToMask s = foldl' (\acc i -> acc .|. (1 `shiftL` i)) 0 indices
  where
    cleaned = map (\c -> if c `elem` ",()" then ' ' else c) s
    indices = map read $ words cleaned

parseLightLine :: String -> (Int, [Int])
parseLightLine line =
  case filter ("[" `isPrefixOf`) parts of
    [] -> error "No lights in line"
    (lights : _) ->
      let buttonStrs = filter ("(" `isPrefixOf`) parts
       in (lightsToMask lights, map buttonToMask buttonStrs)
  where
    parts = words line

minPressesBFS :: Int -> [Int] -> Int
minPressesBFS target buttons = bfs (Set.singleton 0) [0] 0
  where
    bfs :: Set Int -> [Int] -> Int -> Int
    bfs visited frontier n
      | target `elem` frontier = n
      | null nextFrontier = error $ "No solution for target " ++ show target
      | otherwise = bfs visited' nextFrontier (n + 1)
      where
        nextFrontier = [xor m b | m <- frontier, b <- buttons, not (Set.member (xor m b) visited)]
        visited' = foldr Set.insert visited nextFrontier

removeChars :: [Char] -> String -> String
removeChars chars = filter (`notElem` chars)

minUsing :: (Ord b) => (a -> b) -> [a] -> a
minUsing f = foldl1 (\x y -> if f x <= f y then x else y)

parseDiagram :: String -> Int
parseDiagram [] = 0
parseDiagram ('.' : xs) = 2 * parseDiagram xs
parseDiagram ('#' : xs) = 1 + 2 * parseDiagram xs
parseDiagram s = error $ "Invalid wiring diagram: " ++ s

parseButton :: String -> [Int]
parseButton s = map read (splitOn "," s)

parseMachineLine :: String -> Machine
parseMachineLine str = case words (removeChars "[](){}" str) of
  (tStr : rest) ->
    let buttonsLists = map parseButton (init rest)
        buttonsMasks = map (foldl' (\acc b -> acc + 2 ^ b) 0) buttonsLists
        buttonsMaps = map (M.fromList . map (\b -> (b, 1))) buttonsLists
        joltagesList = map read $ splitOn "," $ last rest
     in Machine
          { targetVal = parseDiagram tStr,
            targetStr = tStr,
            buttons = buttonsMasks,
            buttonsMap = buttonsMaps,
            joltages = joltagesList
          }
  _ -> error $ "Unable to parse line: " ++ str

toBits :: Int -> [Int]
toBits n = [i | i <- [0 .. finiteBitSize n - 1], n .&. (1 `shiftL` i) /= 0]

substitute :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
substitute subs (vars, target) =
  foldl apply (vars, target) subs
  where
    apply (v, t) (var, val)
      | v .&. (1 `shiftL` var) /= 0 = (v - (1 `shiftL` var), t - val)
      | otherwise = (v, t)

isValidEq :: (Int, Int) -> Bool
isValidEq (0, 0) = True
isValidEq (0, _) = False
isValidEq (_, v) = v >= 0

machineToEq :: Machine -> (Int, [(Int, Int)])
machineToEq m = (2 ^ length (buttons m) - 1, eqs)
  where
    btns = buttons m
    pairs = zip [0 ..] btns
    bitmaskForIndex i = sum [if b .&. (1 `shiftL` i) /= 0 then 2 ^ j else 0 | (j, b) <- pairs]
    eqs = zipWith (\i t -> (bitmaskForIndex i, t)) [0 ..] (joltages m)

waysToSum :: Int -> Int -> [[Int]]
waysToSum 0 0 = [[]]
waysToSum 0 n = [replicate n 0]
waysToSum _ 0 = []
waysToSum s n = concat [map (a :) $ waysToSum (s - a) (n - 1) | a <- [0 .. s]]

augment :: [(Int, Int)] -> [(Int, Int)]
augment eqs = eqs ++ [(vb - va, tb - ta) | (va, ta) <- eqs, (vb, tb) <- eqs, tb > ta, va .&. vb == va]

minimizeSystem :: (Int, [(Int, Int)]) -> Int
minimizeSystem (0, _) = 0
minimizeSystem (remainingBits, eqs) =
  target + bestOfRest
  where
    chosenEq@(eq, target) = minUsing (popCount . fst) eqs
    bits = toBits eq
    candidates = map (zip bits) $ waysToSum target (length bits)
    otherEqs = filter (/= chosenEq) eqs
    filteredCandidates = [(remainingBits - eq', remainingEqs) | candidate <- candidates, remainingEqs <- [map (substitute candidate) otherEqs], all isValidEq remainingEqs, let eq' = eq]
    bestOfRest =
      if null filteredCandidates
        then maxBound
        else minimum $ map minimizeSystem filteredCandidates

solveMachine :: Machine -> Int
solveMachine m = minimizeSystem (rbs, augment eqs)
  where
    (rbs, eqs) = machineToEq m

part1 :: [String] -> Int
part1 = sum . map (uncurry minPressesBFS . parseLightLine)

part2 :: [String] -> Int
part2 input = sum $ map (solveMachine . parseMachineLine) input

main :: IO ()
main = do
  input <- getInputData 10
  let ls = lines input
  putStrLn $ "Part One: " ++ show (part1 ls)
  putStrLn $ "Part Two: " ++ show (part2 ls)
