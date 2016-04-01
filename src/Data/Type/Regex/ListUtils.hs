{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Regex.ListUtils
  (
    type Nub
  , type IsNull
  , type FilterOut
  , type Elem
  , type (++)
  ) where

-- |Nub elements O(n^2)
type family Nub (xs :: [k]) :: [k] where
    Nub '[] = '[]
    Nub (x ': xs) = x ': Nub (FilterOut x xs)

type family IsNull xs where
    IsNull '[] = 'True
    IsNull xs  = 'False

-- FilterOut x xs ~~ filter (/= x) xs
type family FilterOut (x :: k) (xs :: [k]) :: [k] where
    FilterOut x '[] = '[]
    FilterOut x (x ': xs) = FilterOut x xs
    FilterOut x (y ': xs) = y ': FilterOut x xs

type family Elem (x :: k) (xs :: [k]) where
    Elem x '[] = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs

type family xs ++ ys where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)
