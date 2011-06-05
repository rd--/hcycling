module Weight where

import Data.Ratio

kg_to_lbs :: (Fractional t) => t -> t
kg_to_lbs = (*) 2.204622622

lbs_to_kgs :: (Fractional t) => t -> t
lbs_to_kgs = (*) 0.45359237

{-
map lbs_to_kgs [120,130 .. 200]
-}

avoirdupois_lb :: Rational
avoirdupois_lb = 9%20

troy_lb :: Rational
troy_lb = 3%8

tower_lb :: Rational
tower_lb = 7%20

merchant_lb :: Rational
merchant_lb = 7%16

london_lb :: Rational
london_lb = 7%15

metric_lb :: Rational
metric_lb = 1%2
