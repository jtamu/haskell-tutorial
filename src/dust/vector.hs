{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

data Nat = Zero | Succ Nat

data Vector (n :: Nat) (a :: *) where
    VNil :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
    show VNil = "[]"
    show (VCons x xs) = "[" ++ show x ++ "," ++ show xs ++ "]"

type family Add n m where
    Add 'Zero m = m
    Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = rest
append (VCons a rest) xs = VCons a (append rest xs)

{-|

>>> :kind Add (Succ (Succ Zero)) (Succ Zero)
Add (Succ (Succ Zero)) (Succ Zero) :: Nat

>>> :kind! Add (Succ (Succ Zero)) (Succ Zero)
Add (Succ (Succ Zero)) (Succ Zero) :: Nat
= 'Succ ('Succ ('Succ 'Zero))

>>> append (VCons 1 (VCons 3 VNil)) (VCons 2 VNil)
[3,[1,[2,[]]]]

-}
