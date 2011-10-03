module Cycling.Physiology where

-- * Age predicted heart rate maxima

type HR_MAX = Double->Double

-- | Dr. William Haskell and Dr. Samuel Fox (1970)
--
-- > hr_max_haskell_and_fox 36 == 184
hr_max_haskell_and_fox :: HR_MAX
hr_max_haskell_and_fox age = 220 - age

-- | Robergs R and Landwehr R (2002).
-- \"The Surprising History of the HRmax=220-age Equation\"
-- /Journal of Exercise Physiology/ 5 (2): 1–10
--
-- > round (hr_max_robergs_and_landwehr 36) == 181
hr_max_robergs_and_landwehr :: HR_MAX
hr_max_robergs_and_landwehr age = 205.8 - (0.685 * age)

-- | Londeree and Moeschberger (University of Missouri)
--
-- > hr_max_londeree_and_moeschberger 36 == 180.704
hr_max_londeree_and_moeschberger :: HR_MAX
hr_max_londeree_and_moeschberger age = 206.3 - (0.711 * age)

-- | Miller et al. (Indiana University)
--
-- > hr_max_miller_et_al 36 == 186.4
hr_max_miller_et_al :: HR_MAX
hr_max_miller_et_al age = 217 - (0.85 * age)

-- | Hirofumi Tanaka et al.
-- \"Age-predicted maximal heart rate revisited\"
-- /J Am Coll Cardiol/, 2001; 37:153-156
--
-- > hr_max_tanaka 36 == 182.8
hr_max_tanaka :: HR_MAX
hr_max_tanaka age = 208 - (0.7 * age)

-- | Gellish, Ronald et al (May, 2007)
-- \"Longitudinal Modeling of the Relationship between Age and Maximal HR\".
-- /Medicine & Science in Sports & Exercise/ 39 (5): 822–828.
--
-- > hr_max_oakland_nonlinear 36 == 182.428
hr_max_oakland_nonlinear :: HR_MAX
hr_max_oakland_nonlinear age = 191.5 - (0.007 * age * age)

-- | Reference values obtained during bicycle ergometry (Lund, Sweden).
--
-- > hr_max_lund 36 == 184.34573183940563
hr_max_lund :: HR_MAX
hr_max_lund age = 203.7 / (1 + exp (0.033 * (age - 104.3)))

-- | List of age predicted HR maxima functions.
--
-- > let n = 36 in
-- > map (\f -> round (f n)) hr_max_all == [184,181,181,186,183,182,184]
hr_max_all :: [HR_MAX]
hr_max_all =
    [hr_max_haskell_and_fox
    ,hr_max_robergs_and_landwehr
    ,hr_max_londeree_and_moeschberger
    ,hr_max_miller_et_al
    ,hr_max_tanaka
    ,hr_max_oakland_nonlinear
    ,hr_max_lund]

-- * Heart rate training zones

-- | <http://www8.garmin.com/manuals/Edge500_OwnersManual.pdf>
--
-- > let f (i,j) = (floor i,floor j)
-- > in map f (hr_zones_garmin 184) == [(92,110),(110,128),(128,147),(147,165),(165,184)]
hr_zones_garmin :: (Fractional a) => a -> [(a,a)]
hr_zones_garmin hr_max =
    let z = map (* hr_max) [0.5,0.6,0.7,0.8,0.9,1.0]
    in zip z (tail z)

-- | <http://www.cptips.com/hrmntr.htm>
--
-- > let f (i,j) = (floor i,floor j)
-- > in map f (hr_zones_ctips 184) == [(92,119),(119,132),(134,147),(154,165),(167,184)]
hr_zones_ctips :: (Fractional a) => a -> [(a,a)]
hr_zones_ctips hr_max =
    let a = [(0.5,0.65),(0.65,0.72),(0.73,0.80),(0.84,0.90),(0.91,1.0)]
        f (i,j) = (i * hr_max,j * hr_max)
    in map f a

-- > hr_zones_zoladz 184 == [(129,139),(139,149),(149,159),(159,169),(169,179)]
hr_zones_zoladz :: (Num a) => a -> [(a, a)]
hr_zones_zoladz hr_max =
    let adj = [50,40,30,20,10]
    in map (\x -> (hr_max - x - 5,hr_max - x + 5)) adj

-- > map (hr_target_karvonen 54 184) [0.5,0.85] == [119.0,164.5]
hr_target_karvonen :: (Num a) => a -> a -> a -> a
hr_target_karvonen hr_rest hr_max n = ((hr_max - hr_rest) * n) + hr_rest

-- * Basal metabolic rate

-- | Synonym for 'Double'.
type R = Double

cal_th_to_joules :: R
cal_th_to_joules = 4.184

-- | Dr. J.A. Harris and Dr. F.G. Benedict (closed circuit spirometry)
--
-- > bmr_harris_benedict_1919_men 63 176 35 == 2049.8296
bmr_harris_benedict_1919_men :: R -> R -> R -> R
bmr_harris_benedict_1919_men w h a =
    let w' = 13.7516 * w
        h' = 5.0033 * h
        a' = 6.7550 * a
    in 66.473 + w' + h' + a'

-- | Enumeration of 'Gender' for /bmr/ calculation.
data Gender = Male | Female

-- | Mifflin, St Jeor et al (1990). \"A new predictive equation for
-- resting energy expenditure in healthy individuals\".
-- /American Journal of Clinical Nutrition/ 51 (2): 241-247.
--
-- > bmr_mifflin_1990 63 176 35 Male == 1910
bmr_mifflin_1990 :: (Fractional t) => t -> t -> t -> Gender -> t
bmr_mifflin_1990 w h a g =
    let w' = 10 * w
        h' = 6.25 * h
        a' = 5 * a
        g' = case g of
               Male -> 5
               Female -> (-161)
    in w' + h' + a' + g'
