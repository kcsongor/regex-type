{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Type.Regex
  (
    type (~=)
  , type (:|)
  , type (:>)
  , type Rep
  , type Opt
  , type Plus
  , type Null
  ) where

import Data.Type.Bool
import GHC.TypeLits
import Data.Type.Regex.ListUtils

-- Matching
class input ~= re
instance (Accepts (MakeNDA re) input ~ 'True) => input ~= re

type family ToTerm (a :: k) :: RE where
  ToTerm (a :: *)  = 'Term a
  ToTerm (a :: RE) = a

type a :| b = 'Alt (ToTerm a) (ToTerm b)
type a :> b = 'Seq (ToTerm a) (ToTerm b)
type Rep a  = 'Rep (ToTerm a)
type Opt a  = 'Alt (ToTerm a) 'Null
type Plus a = 'Seq (ToTerm a) ('Rep (ToTerm a))
type Null   = 'Null

-- PRIVATE:
data RE
  = Null
  | forall (c :: *). Term c
  | Seq RE RE
  | Alt RE RE
  | Rep RE

data Label
  = forall (c :: *). C c
  | Eps

-- check if nda accepts input
type family Accepts (automaton :: (Nat, [Nat], [(Nat, Nat, Label)])) (input :: [*]) :: Bool where
  Accepts '(start, terminals, ts) i
    = Accepts' start '(start, terminals, ts) i

type family Accepts' (state :: Nat) (automaton :: (Nat, [Nat], [(Nat, Nat, Label)])) (input :: [*]) :: Bool where
  Accepts' start a input
    = (IsTerminal start a && IsNull input)
      || AnyAccepted input a (TransitionsFromTo start (AllTransitions a))

type family IsTerminal state automaton :: Bool where
  IsTerminal s '(start, terminals, ts)
    = Elem s terminals

type family TransitionsFromTo state (ts :: [(Nat, Nat, Label)]) :: [(Nat, Nat, Label)] where
  TransitionsFromTo state '[] = '[]
  TransitionsFromTo state ('(state, to, label) ': ts)
    = '(state, to, label) ': TransitionsFromTo state ts
  TransitionsFromTo state (t ': ts)
    = TransitionsFromTo state ts

type family AllTransitions automaton :: [(Nat, Nat, Label)] where
  AllTransitions '(start, term, all)
    = all

type family LabelsOf (ts :: [(Nat, Nat, Label)]) :: [Label] where
  LabelsOf ts
    = Nub (FilterOut 'Eps (LabelsOf' ts))

type family LabelsOf' (ts :: [(Nat, Nat, Label)]) :: [Label] where
  LabelsOf' '[]
    = '[]
  LabelsOf' ('(from, to, label) ': ts)
    = label ': LabelsOf' ts

type family AnyAccepted input automaton transitions :: Bool where
  AnyAccepted i a '[]
    = 'False
  AnyAccepted i a (t ': ts)
    = Try i a t || AnyAccepted i a ts

type family Try input automaton transition :: Bool where
  Try input a '(from, to, 'Eps)
    = Accepts' to a input
  Try (h ': rest) a '(from, to, 'C h)
    = Accepts' to a rest
  Try i a t
    = 'False

-- Make NDA

type family Fst (tuple :: (a, b)) :: a where
  Fst '(a, b) = a

type family Snd (tuple :: (a, b)) :: b where
  Snd '(a, b) = b

type family MakeNDA (re :: RE) :: (Nat, [Nat], [(Nat, Nat, Label)]) where
  MakeNDA re
    = '(1, '[2], Fst (Make re 1 2 3))

type family Make (re :: RE) (m :: Nat) (n :: Nat) (k :: Nat) :: ([(Nat, Nat, Label)], Nat) where
  Make 'Null m n k
    = '( '[ '(m, n, 'Eps)], k)
  Make ('Term c) m n k
    = '( '[ '(m, n, 'C c)], k)
  Make ('Seq re1 re2) m n k
    = Make re1 m k (k + 2)
      `BindMake` '(re2, (k + 1), n)
      `BindMake` '( 'Null, k, (k + 1))
  Make ('Alt re1 re2) m n k
    = Make re1 k (k + 1) (k + 4)
      `BindMake` '(re2, (k + 2), (k + 3))
      `Comb1` Make 'Null m k (k + 1)
      `Comb1` Make 'Null m (k + 2) (k + 3)
      `Comb1` Make 'Null (k + 1) n n
      `Comb1` Make 'Null (k + 3) n n
  Make ('Rep re) m n k
    = Make re k (k + 1) (k + 2)
      `Comb1` Make 'Null m k k
      `Comb1` Make 'Null (k + 1) k k
      `Comb1` Make 'Null (k + 1) n n
      `Comb1` Make 'Null m n n

type family BindMake (ma :: ([(Nat, Nat, Label)], Nat)) (mba :: (RE, Nat, Nat)) :: ([(Nat, Nat, Label)], Nat) where
  BindMake '(ts1, k1) '(re, m, n)
    = Comb2 '(ts1, k1) (Make re m n k1)

type family Comb1 (a :: ([t], k1)) (b :: ([t], k2)) :: ([t], k1) where
  Comb1 '(ts1, k1) '(ts2, k2)
    = '(ts1 ++ ts2, k1)

type family Comb2 (a :: ([t], k1)) (b :: ([t], k2)) :: ([t], k2) where
  Comb2 '(ts1, k1) '(ts2, k2)
    = '(ts1 ++ ts2, k2)
