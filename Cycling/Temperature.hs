-- | Temperature unit conversion
module Cycling.Temperature where

import Data.Ratio

fahrenheit_to_celsius :: Rational -> Rational
fahrenheit_to_celsius = (*) (5%9) . (+ (- 32))

celsius_to_fahrenheit :: Rational -> Rational
celsius_to_fahrenheit = (+ 32) . (*) (9%5)

_r :: (Real a,Fractional b) => (Rational -> Rational) -> a -> b
_r f = fromRational . f . toRational

fahrenheit_to_celsius_r :: (Real t,Fractional t) => t -> t
fahrenheit_to_celsius_r = _r fahrenheit_to_celsius

celsius_to_fahrenheit_r :: (Real t,Fractional t) => t -> t
celsius_to_fahrenheit_r = _r celsius_to_fahrenheit
