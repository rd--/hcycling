-- | Temperature unit conversion
module Cycling.Temperature where

import Data.Ratio {- base -}

{- | F to C

>>> round (fahrenheit_to_celsius 100)
38
-}
fahrenheit_to_celsius :: Rational -> Rational
fahrenheit_to_celsius = (*) (5 % 9) . (+ (-32))

{- | F to C

>>> round (celsius_to_fahrenheit 38)
100
-}
celsius_to_fahrenheit :: Rational -> Rational
celsius_to_fahrenheit = (+ 32) . (*) (9 % 5)

_r :: (Real a, Fractional b) => (Rational -> Rational) -> a -> b
_r f = fromRational . f . toRational

{- | F to C (R)

>>> fahrenheit_to_celsius_r 100.4
38.0
-}
fahrenheit_to_celsius_r :: (Real t, Fractional t) => t -> t
fahrenheit_to_celsius_r = _r fahrenheit_to_celsius

{- | C to F (R)

>>> celsius_to_fahrenheit_r 38
100.4
-}
celsius_to_fahrenheit_r :: (Real t, Fractional t) => t -> t
celsius_to_fahrenheit_r = _r celsius_to_fahrenheit
