module Day01 (day01) where

import qualified Data.IntMultiSet as IMS
import qualified Data.List as L
import qualified Data.Text as T

type Input = ([Int], [Int])

parseLine :: T.Text -> (Int, Int)
parseLine line = let (a : b : xs) = map (read . T.unpack) . T.words $ line in (a, b)

parse :: T.Text -> Input
parse = unzip . map parseLine . filter (not . T.null) . T.lines

part1 :: Input -> Int
part1 (a, b) =
  let pairs = zip (L.sort a) (L.sort b)
   in sum $ abs . uncurry (-) <$> pairs

part2 :: Input -> Int
part2 (a, b) =
  let occurrences = IMS.fromList b
   in sum $ map (\v -> v * IMS.occur v occurrences) a

day01 :: (T.Text -> Input, Input -> Int, Input -> Int)
day01 = (parse, part1, part2)
