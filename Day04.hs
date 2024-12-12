module Day04 (day04) where

import qualified Data.Text as T
import qualified Grid2d as G
import qualified Point2d as P

type Input = G.Grid2d Char

parseInput :: T.Text -> Input
parseInput = G.fromText

match1 :: P.Direction2d -> P.Point2d -> Input -> Int
match1 direction origin input =
  let string = map fst . take 4 . G.raycast direction origin $ input
   in if string == "XMAS" || string == "SAMX" then 1 else 0

matches1 :: P.Point2d -> Input -> Int
matches1 origin input =
  let horiz = match1 P.East origin input
      vert = match1 P.South origin input
      diag45 = match1 P.Southeast origin input
      diag135 = match1 P.Southwest origin input
   in horiz + vert + diag45 + diag135

match2 :: P.Direction2d -> P.Point2d -> Input -> Bool
match2 direction origin input =
  let string = map fst . take 3 . G.raycast direction origin $ input
   in string == "MAS" || string == "SAM"

matches2 :: P.Point2d -> Input -> Int
matches2 origin input =
  let a = match2 P.Southeast (origin `P.add` P.delta P.Northwest) input
      b = match2 P.Northeast (origin `P.add` P.delta P.Southwest) input
   in if a && b then 1 else 0

solve :: (P.Point2d -> Input -> Int) -> Input -> Int
solve f input = sum . map (`f` input) $ G.coords input

part1 :: Input -> Int
part1 = solve matches1

part2 :: Input -> Int
part2 = solve matches2

day04 = (parseInput, part1, part2)
