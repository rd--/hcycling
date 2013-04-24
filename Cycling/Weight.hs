module Cycling.Weight where

import Data.Ratio {- base -}

-- | Conversion factor from /lb/s to kilograms.
lb :: (Fractional t) => t
lb = 0.45359237

-- | Convert from kilograms to /lb/s.
--
-- > map (round.kg_to_lbs) [59,68,77,86] == [130,150,170,190]
kg_to_lbs :: (Fractional t) => t -> t
kg_to_lbs = (*) (recip lb)

-- | Convert from /lb/s to kilograms.
--
-- > map (round.lbs_to_kgs) [120,130 .. 200] == [54,59,64,68,73,77,82,86,91]
lbs_to_kgs :: (Fractional t) => t -> t
lbs_to_kgs = (*) lb

avoirdupois_lb :: Rational
avoirdupois_lb = 9%20

troy_lb :: (Fractional t) => t
troy_lb = 0.3732417216

troy_lb_approx :: Rational
troy_lb_approx = 3%8

tower_lb_approx :: Rational
tower_lb_approx = 7%20

merchant_lb_approx :: Rational
merchant_lb_approx = 7%16

london_lb_approx :: Rational
london_lb_approx = 7%15

metric_lb_approx :: Rational
metric_lb_approx = 1%2
