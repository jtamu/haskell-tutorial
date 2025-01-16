{-# LANGUAGE TemplateHaskell #-}

module Atom where

import Control.Lens (over, view)
import Control.Lens.TH (makeLenses)

data Atom = Atom {_element :: String, _point :: Point} deriving (Show)

data Point = Point {_x :: Double, _y :: Double} deriving (Show)

newtype Molecule = Molecule {_atoms :: [Atom]} deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)
$(makeLenses ''Molecule)

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1.0)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1.0)

viewAtomX :: Atom -> Double
viewAtomX = view (point . x)

{-

>>> let atom1 = Atom {_element = "H", _point = Point {_x = 1.0, _y = 2.0}}
>>> shiftAtomX atom1
Atom {_element = "H", _point = Point {_x = 2.0, _y = 2.0}}

>>> let atom2 = Atom {_element = "O", _point = Point {_x = 3.0, _y = 4.0}}
>>> let molecule = Molecule {_atoms = [atom1, atom2]}
>>> shiftMoleculeX molecule
Molecule {_atoms = [Atom {_element = "H", _point = Point {_x = 2.0, _y = 2.0}},Atom {_element = "O", _point = Point {_x = 4.0, _y = 4.0}}]}

>>> viewAtomX atom1
1.0

-}
