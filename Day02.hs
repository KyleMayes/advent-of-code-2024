module Day02 (day02) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Utility (windows)

type Input = [[Int]]

parseInput :: T.Text -> Input
parseInput = map (map (read . T.unpack) . T.words) . filter (not . T.null) . T.lines

safe1 :: [Int] -> Bool
safe1 report =
  let deltas = map (\[a, b] -> a - b) $ windows 2 report
      up = all (> 0) deltas
      down = all (< 0) deltas
      range = all ((\d -> d >= 1 && d <= 3) . abs) deltas
   in (up || down) && range

dropped :: [a] -> [a] -> [[a]]
dropped _ [] = []
dropped left (y : ys) = (left ++ ys) : dropped (left ++ [y]) ys

safe2 :: [Int] -> Bool
safe2 report = safe1 report || any safe1 (dropped [] report)

part1 :: Input -> Int
part1 = length . filter safe1

part2 :: Input -> Int
part2 = length . filter safe2

day02 = (parseInput, part1, part2)
