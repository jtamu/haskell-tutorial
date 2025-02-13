{-# OPTIONS -Wall -Werror #-}

module Geometry.Cube
  ( volume,
    area,
  )
where

import Geometry.Cuboid qualified as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
