# regex-type
Regular expression matching of Haskell types using a nondeterministic finite automata

*This is a playground for writing type-level code using TypeFamilies.*

Some examples that work:

```haskell
-- encode the following regex: (Int | Char (Bool -> Bool) (String | (Int -> Bool))
regex :: ('[a, b] ~= ((Int :| Char :| (Bool -> Bool)) :> (String :| (Int -> Bool)))) => a -> b
regex = undefined

-- The type of these functions all match the regular expression defined in the type of regex
-- so they all typecheck
test1 :: Int -> String
test1 = regex

test2 :: Int -> Int -> Bool
test2 = regex

test3 :: Char -> String
test3 = regex

test4 :: Char -> Int -> Bool
test4 = regex

test5 :: (Bool -> Bool) -> Int -> Bool
test5 = regex

test6 :: (Bool -> Bool) -> String
test6 = regex

-- This doesn't satisfy the regex, and thus a type error occurs
test_wrong :: Int -> Bool
test_wrong = regex
```

# TODO:
- Improve performance
- Make it a proper library
- Try converting the NFA to NDA. I don't know if this will make it faster overall, as I suspect the automaton is reconstructed every time, which means an exponential complexity on each check with the DFA. Maybe an on-demand construction?
- Figure out if it's actually useful for anything