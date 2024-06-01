module Cycling.Physiology where

-- * Basal metabolic rate

-- | Synonym for 'Double'.
type R = Double

cal_th_to_joules :: R
cal_th_to_joules = 4.184

{- | Dr. J.A. Harris and Dr. F.G. Benedict (closed circuit spirometry)

>>> bmr_harris_benedict_1919_men 63 176 35
2049.8296
-}
bmr_harris_benedict_1919_men :: R -> R -> R -> R
bmr_harris_benedict_1919_men w h a =
  let w' = 13.7516 * w
      h' = 5.0033 * h
      a' = 6.7550 * a
  in 66.473 + w' + h' + a'

-- | Enumeration of 'Gender' for /bmr/ calculation.
data Gender = Male | Female

{- | Mifflin, St Jeor et al (1990). \"A new predictive equation for
resting energy expenditure in healthy individuals\".
/American Journal of Clinical Nutrition/ 51 (2): 241-247.

>>> bmr_mifflin_1990 63 176 35 Male
1910.0
-}
bmr_mifflin_1990 :: (Fractional t) => t -> t -> t -> Gender -> t
bmr_mifflin_1990 w h a g =
  let w' = 10 * w
      h' = 6.25 * h
      a' = 5 * a
      g' = case g of
        Male -> 5
        Female -> -161
  in w' + h' + a' + g'
