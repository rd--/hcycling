{-# Language ExistentialQuantification #-}
module Cycling.Solve where

-- | Iterative solver data.
data Solve x a =
    (Fractional x,Num a,Ord a) =>
    Solve {function :: x -> a -- ^ 'input' -> 'output' function to solve
          ,tolerance :: a -- ^ accepted tolerance at 'output'
          ,output :: a -- ^ required output value
          ,input :: x -- ^ initial input value
          ,increment :: x -- ^ initial value to shift 'input'
          }

solve' :: Ordering -> Solve x a -> (x,a)
solve' d (Solve f t o i n) =
    let o' = f i
        d' = compare o' o
        n' = if d == d' then n else n / 2
    in if abs (o - o') < t
       then (i,o')
       else case d' of
              EQ -> (i,o')
              GT -> solve' d' (Solve f t o (i - n') n')
              LT -> solve' d' (Solve f t o (i + n') n')

-- | Very simple one-dimensional interative solver.  Returns both the
-- solution for 'input' and the actual 'output' value (which will lie
-- within specified 'tolerance').
--
-- > solve (Solve (* 2) 0 10 20 4) == (5,10)
solve :: Solve x a -> (x, a)
solve = solve' EQ
