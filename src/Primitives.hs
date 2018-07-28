module Primitives where

import Lib

-- | Constructs a sphere centered at the origin given a radius,
-- radial subdivisions, and vertical subdivisions.
sphere :: GU -> Int -> Int -> Vertex
sphere 0 _ _ = error "radius cannot be 0"
sphere radius radialSubs verticalSubs
  | radius <= 0 = error "invalid radius"
  | radialSubs < 3 = error "invalid subdivisions"
  | verticalSubs < 3 = error "invalid subdivisions"