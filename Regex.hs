{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Regex where

import Data.Proxy
import GHC.TypeLits

data Label
  = forall (c :: *). C c
  | Eps

type family Accepts (automaton :: (Nat, [Nat], [(Nat, Nat, Label)])) (input :: [*]) :: Bool where
    Accepts '(start, terminals, ts) i
          = Accepts' start '(start, terminals, ts) i

type family Accepts' (state :: Nat) (automaton :: (Nat, [Nat], [(Nat, Nat, Label)])) (input :: [*]) :: Bool where
    Accepts' start input a
          = 'True

