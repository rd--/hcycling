module Cycling.Solve (solve) where

solve :: (Fractional x, Num a, Ord a) =>
         (x -> a) -> a -> a -> x -> (x, Ordering) -> (x, a)
solve f t o i (n, d) =
    let o' = f i
        d' = compare o' o
        n' = if d == d' then n else n / 2
    in if abs (o - o') < t
       then (i, o')
       else case d' of
              EQ -> (i, o')
              GT -> solve f t o (i - n') (n', d')
              LT -> solve f t o (i + n') (n', d')
