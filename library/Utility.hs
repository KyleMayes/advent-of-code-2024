module Utility
  ( windows,
  )
where

import qualified Data.Vector as V

windows :: Int -> [a] -> [[a]]
windows size list =
  let vector = V.fromList list
      indices = [0 .. (V.length vector - size)]
   in map (\i -> V.toList $ V.slice i size vector) indices
