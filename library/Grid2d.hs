module Grid2d
  ( Grid2d (..),
    fromList,
    toList,
    fromText,
    toString,
    inside,
    (!),
    (!?),
    rows,
    columns,
    coords,
    raycast,
  )
where

import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Point2d as P

data Grid2d a = Grid2d
  { width :: Int,
    height :: Int,
    cells :: V.Vector a
  }
  deriving (Eq, Functor, Show)

fromList :: [[a]] -> Grid2d a
fromList lists =
  let width = length . head $ lists
      height = length lists
   in Grid2d width height (V.fromList . concat $ lists)

toList :: Grid2d a -> [[a]]
toList = rows

fromText :: T.Text -> Grid2d Char
fromText = fromList . map T.unpack . filter (not . T.null) . T.lines

toString :: (a -> Char) -> Grid2d a -> String
toString f = intercalate "\n" . rows . fmap f

inside :: Grid2d a -> P.Point2d -> Bool
inside grid (x, y) = x >= 0 && x < width grid && y >= 0 && y < height grid

(!) :: Grid2d a -> P.Point2d -> a
(!) grid (x, y) = cells grid V.! ((y * width grid) + x)

(!?) :: Grid2d a -> P.Point2d -> Maybe a
(!?) grid (x, y) = cells grid V.!? ((y * width grid) + x)

rows :: Grid2d a -> [[a]]
rows grid =
  let xs = [0 .. width grid - 1]
      ys = [0 .. height grid - 1]
   in map (\y -> map (\x -> grid ! (x, y)) xs) ys

columns :: Grid2d a -> [[a]]
columns grid =
  let xs = [0 .. width grid - 1]
      ys = [0 .. height grid - 1]
   in map (\x -> map (\y -> grid ! (x, y)) ys) xs

coords :: Grid2d a -> [P.Point2d]
coords grid =
  let xs = [0 .. width grid - 1]
      ys = [0 .. height grid - 1]
   in concatMap (\y -> map (,y) xs) ys

raycast :: P.Direction2d -> P.Point2d -> Grid2d a -> [(a, P.Point2d)]
raycast direction origin grid =
  let points = takeWhile (inside grid) $ P.raycast direction origin
   in map (\point -> (grid ! point, point)) points
