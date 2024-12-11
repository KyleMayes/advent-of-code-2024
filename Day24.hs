module Day24 (day24) where

import qualified Data.Text as T

type Input = [T.Text]

parseInput :: T.Text -> Input
parseInput = filter (not . T.null) . T.lines

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

day24 = (parseInput, part1, part2)
