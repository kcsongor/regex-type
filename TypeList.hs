{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeList where

import Data.Type.Bool
import GHC.TypeLits

-- * Type-level list utilities

-- |Nub elements O(n^2)
type family Nub (xs :: [k]) :: [k] where
    Nub '[] = '[]
    Nub (x ': xs) = x ': Nub (FilterOut x xs)

--nubTest :: Proxy (Nub '[1,2,5,2,4,7,6,3,4,2,4,1,3,8,9,5,3])
--nubTest = Proxy

type family Null xs where
    Null '[] = 'True
    Null xs  = 'False

-- FilterOut x xs ~~ filter (/= x) xs
type family FilterOut (x :: k) (xs :: [k]) :: [k] where
    FilterOut x '[] = '[]
    FilterOut x (x ': xs) = FilterOut x xs
    FilterOut x (y ': xs) = y ': FilterOut x xs

--filterOutTest :: Proxy (FilterOut 4 '[1,2,3,4,5,6,7])
--filterOutTest = Proxy

type family Elem (x :: k) (xs :: [k]) where
    Elem x '[] = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs

type family xs ++ ys where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

-- TODO: implement quicksort
-- Sort type level lists in O(n^2) using Less as a comparator
type family Less (a :: k) (b :: k) :: Bool

-- TODO: more instances (at least the tuples)
type instance Less (a :: Nat) b             = a <=? b
type instance Less '(a, b) '(a', b')        = a `Less` a'
type instance Less '(a, b, c) '(a', b', c') = a `Less` a'


type family Min' (xs :: [k]) min where
    Min' '[] min = min
    Min' (x ': xs) min = If (x `Less` min) (Min' xs x) (Min' xs min)

type family Min (xs :: [k]) where
    Min (x ': xs) = Min' xs x

type family Sort (xs :: [k]) where
    Sort '[] = '[]
    Sort (x ': xs) = Sort' (x ': xs) (Min' xs x)

type family Sort' (xs :: [k]) min where
    Sort' xs min = min ': Sort (RemoveFirst xs min)

type family RemoveFirst xs x where
    RemoveFirst '[] x = '[]
    RemoveFirst (x ': xs) x = xs
    RemoveFirst (y ': xs) x = y ': RemoveFirst xs x

