module Point2d
  ( Point2d (..),
    add,
    sub,
    Direction2d (..),
    delta,
    raycast,
  )
where

type Point2d = (Int, Int)

add :: Point2d -> Point2d -> Point2d
add (ax, ay) (bx, by) = (ax + bx, ay + by)

sub :: Point2d -> Point2d -> Point2d
sub (ax, ay) (bx, by) = (ax - bx, ay - by)

mul :: Int -> Point2d -> Point2d
mul n (x, y) = (n * x, n * y)

data Direction2d
  = North
  | East
  | South
  | West
  | Northeast
  | Southeast
  | Southwest
  | Northwest
  deriving (Eq, Show)

delta :: Direction2d -> Point2d
delta North = (0, -1)
delta East = (1, 0)
delta South = (0, 1)
delta West = (-1, 0)
delta Northeast = (1, -1)
delta Southeast = (1, 1)
delta Southwest = (-1, 1)
delta Northwest = (-1, -1)

raycast :: Direction2d -> Point2d -> [Point2d]
raycast direction origin = origin : raycast direction (origin `add` delta direction)
