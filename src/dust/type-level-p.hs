{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Proxy
import Data.Kind (Type)

data PeonoNat = Zero | Succ PeonoNat deriving Show

type family Pred (n :: PeonoNat) :: PeonoNat where
    Pred 'Zero = 'Zero
    Pred ('Succ n) = n

type family Add (n :: PeonoNat) (m :: PeonoNat) :: PeonoNat where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)

type family (n :: PeonoNat) + (m :: PeonoNat) :: PeonoNat where
    'Zero + n = n
    ('Succ n) + m = 'Succ (n + m)

type family IsInt (t :: Type) :: Bool where
    IsInt Int = 'True
    IsInt a = 'False

-- newtype SizedList (n :: PeonoNat) a = SizedList [a]

-- safeHead :: SizedList ('Succ m) a -> a
-- safeHead (SizedList xs) = head xs

data SizedList (n :: PeonoNat) a where
    Nil :: SizedList 'Zero a
    Cons :: a -> SizedList n a -> SizedList ('Succ n) a

data SomeSizedList a where
    SomeSizedList :: SizedList n a -> SomeSizedList a

filters :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filters f Nil = SomeSizedList Nil
filters f (Cons x xs)
    | f x       = Cons x (filters f xs)
    | otherwise = filters f xs

class PeonoNatToInteger (n :: PeonoNat) where
    peonoNatToInteger :: Proxy n -> Integer

instance PeonoNatToInteger 'Zero where
    peonoNatToInteger _ = 0

instance PeonoNatToInteger n => PeonoNatToInteger ('Succ n) where
    peonoNatToInteger _ = 1 + peonoNatToInteger (Proxy :: Proxy n)

lengthOfSizedList :: forall n. PeonoNatToInteger n => SizedList n a -> Integer
lengthOfSizedList _ = peonoNatToInteger (Proxy :: Proxy n)

data SPeanoNat (n :: PeonoNat) where
    SZero :: SPeanoNat 'Zero
    SSucc :: SPeanoNat n -> SPeanoNat ('Succ n)

-- シングルトン型
sAdd :: SPeanoNat n -> SPeanoNat m -> SPeanoNat (Add n m)
sAdd SZero m = m
sAdd (SSucc n) m = SSucc (sAdd n m)

data SBool (x :: Bool) where
    STrue :: SBool 'True
    SFalse :: SBool 'False

data SOrdering (x :: Ordering) where
    SLT :: SOrdering 'LT
    SEQ :: SOrdering 'EQ
    SGT :: SOrdering 'GT

type family Compare (n :: PeonoNat) (m :: PeonoNat) :: PeonoNat where
    Compare 'Zero 'Zero = 'EQ
    Compare 'Zero ('Succ m) = 'LT
    Compare ('Succ n) 'Zero = 'GT
    Compare ('Succ n) ('Succ m) = Compare n m

sCompare :: SPeanoNat n -> SPeanoNat m -> SOrdering (Compare n m)
sCompare SZero SZero = SEQ
sCompare SZero (SSucc m) = SLT
sCompare (SSucc n) SZero = SGT
sCompare (SSucc n) (SSucc m) = sCompare n m

-- data Expr a = Const Int
--          | Add (Expr Int) (Expr Int)
--          | Equal (Expr Int) (Expr Int)
--          | IfThenElse (Expr Bool) (Expr a) (Expr a)
--           deriving Show

-- mkConst :: Int -> Expr Int
-- mkConst = Const

-- mkAdd :: Expr Int -> Expr Int -> Expr Int
-- mkAdd = Add

-- mkEqual :: Expr Int -> Expr Int -> Expr Bool
-- mkEqual = Equal

-- mkIfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
-- mkIfThenElse = IfThenElse

data Expr (a :: Type) where
    Const :: Int -> Expr Int
    Add :: Expr Int -> Expr Int -> Expr Int
    Equal :: Expr Int -> Expr Int -> Expr Bool
    IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval e = case e of
    Const x -> x
    Add e1 e2 -> eval e1 + eval e2
    Equal e1 e2 -> eval e1 == eval e2
    IfThenElse e t f -> if eval e then eval t else eval f

maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Proxy a -> Integer
maxBoundAsInteger _proxy = toInteger (maxBound :: a)

{-|

>>> maxBoundAsInteger (Proxy :: Proxy Int)
9223372036854775807

>>> :kind! Pred 'Zero
Pred 'Zero :: PeonoNat
= 'Zero

>>> :kind! Pred ('Succ ('Succ 'Zero))
Pred ('Succ ('Succ 'Zero)) :: PeonoNat
= 'Succ 'Zero

>>> :kind! Add ('Succ ('Succ 'Zero)) ('Succ 'Zero)
Add ('Succ ('Succ 'Zero)) ('Succ 'Zero) :: PeonoNat
= 'Succ ('Succ ('Succ 'Zero))

>>> kind! ('Succ ('Succ 'Zero)) + ('Succ 'Zero)
Syntax error on 'Succ
Perhaps you intended to use TemplateHaskell or TemplateHaskellQuotes
In the Template Haskell quotation 'Succ

>>> peonoNatToInteger (Proxy :: Proxy ('Succ ('Succ ('Succ 'Zero))))
3

>>> :kind! IsInt Int
IsInt Int :: Bool
= 'True

>>> :kind! IsInt Bool
IsInt Bool :: Bool
= 'False

-}
