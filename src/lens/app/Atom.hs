{-# LANGUAGE TemplateHaskell #-}

module Atom where

import Control.Lens (over)
import Control.Lens.TH (makeLenses)

data Atom = Atom {_element :: String, _point :: Point} deriving (Show)

data Point = Point {_x :: Double, _y :: Double} deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1.0)

{-

>>> let atom = Atom {_element = "H", _point = Point {_x = 1.0, _y = 2.0}}
>>> shiftAtomX atom
Atom {_element = "H", _point = Point {_x = 2.0, _y = 2.0}}

-}
