module NewEden.Closest
    (
      between
    , distanceToLine
    ) where

import NewEden.Types

import Data.List

cross :: Coordinate -> Coordinate -> Coordinate
cross (Coordinate x0 y0 z0) (Coordinate x1 y1 z1) =
    Coordinate
        (y0*z1 - z0*y1)
        (z0*x1 - x0*z1)
        (x0*y1 - y0*x1)

cmp :: Coordinate -> (Celestial,Celestial) -> (Celestial,Celestial) -> Ordering
cmp p (a1,a2) (b1,b2) =
    let
        a = (celestialCoord a1, celestialCoord a2)
        b = (celestialCoord b1, celestialCoord b2)
    in
    compare (distanceToLine p a) (distanceToLine p b)

distanceToLine :: Coordinate -> (Coordinate, Coordinate) -> Lightyear
distanceToLine p (a,b) =
    -- http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
    (norm (cross (diff b a) (diff a p))) / ((norm (diff b a)) ** 2)

-- | Returns a pairs of celestials ordered by how close the given coordinate
-- is from the line between the two celestials.
--
-- Note this here is clearly O(n^2).
--
between :: Solarsystem -> Coordinate -> [(Celestial, Celestial)]
between system coordinate =
    let
        celestials = permutations $ systemCelestials system
    in
    sortBy (cmp coordinate) celestials
    where
        permutations :: [Celestial] -> [(Celestial, Celestial)]
        permutations [] = []
        permutations (x:xs) =
            (zip (repeat x) xs) ++ permutations xs
