module Cycling.HR where

import Data.Bifunctor {- bifunctors -}
import Data.List {- base -}
import Data.Maybe {- base -}

-- * Age predicted heart rate maxima

type HR_MAX = Double->Double

-- | Dr. William Haskell and Dr. Samuel Fox (1970)
--
-- > map (round . hr_max_haskell_and_fox) [36,40] == [184,180]
hr_max_haskell_and_fox :: HR_MAX
hr_max_haskell_and_fox age = 220 - age

-- | Robergs R and Landwehr R (2002).
-- \"The Surprising History of the HRmax=220-age Equation\"
-- /Journal of Exercise Physiology/ 5 (2): 1–10
--
-- > map (round . hr_max_robergs_and_landwehr) [36,40] == [181,178]
hr_max_robergs_and_landwehr :: HR_MAX
hr_max_robergs_and_landwehr age = 205.8 - (0.685 * age)

-- | Londeree and Moeschberger (University of Missouri)
--
-- > map (round . hr_max_londeree_and_moeschberger) [36,40] == [181,178]
hr_max_londeree_and_moeschberger :: HR_MAX
hr_max_londeree_and_moeschberger age = 206.3 - (0.711 * age)

-- | Miller et al. (Indiana University)
--
-- > map (round . hr_max_miller_et_al) [36,40] == [186,183]
hr_max_miller_et_al :: HR_MAX
hr_max_miller_et_al age = 217 - (0.85 * age)

-- | Hirofumi Tanaka et al.
-- \"Age-predicted maximal heart rate revisited\"
-- /J Am Coll Cardiol/, 2001; 37:153-156
--
-- > map (round . hr_max_tanaka) [36,40] == [183,180]
hr_max_tanaka :: HR_MAX
hr_max_tanaka age = 208 - (0.7 * age)

-- | Gellish, Ronald et al (May, 2007)
-- \"Longitudinal Modeling of the Relationship between Age and Maximal HR\".
-- /Medicine & Science in Sports & Exercise/ 39 (5): 822–828.
--
-- > map (round . hr_max_oakland_nonlinear) [36,40] == [182,180]
hr_max_oakland_nonlinear :: HR_MAX
hr_max_oakland_nonlinear age = 191.5 - (0.007 * age * age)

-- | Reference values obtained during bicycle ergometry (Lund, Sweden).
--
-- > map (round . hr_max_lund) [36,40] == [184,182]
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

-- | 'zip' with 'tail'.
--
-- > zones_bounded [0,50,75,100] == [(0,50),(50,75),(75,100)]
zones_bounded :: [t] -> [(t,t)]
zones_bounded z = zip z (tail z)

zones_bounded_hrm :: Fractional t => t -> [t] -> [(t,t)]
zones_bounded_hrm hrm z = zones_bounded (map ((/ 100) . (* hrm)) z)

zone_round :: (RealFrac n,Integral i) => (n,n) -> (i,i)
zone_round = bimap ceiling floor

-- | Rounded variant.
zones_bounded_hrm' :: (Integral i, RealFrac n) => n -> [n] -> [(i,i)]
zones_bounded_hrm' hrm = map zone_round . zones_bounded_hrm hrm

zone_lookup :: Ord a => [a] -> a -> Maybe Int
zone_lookup z x = findIndex (\(p,q) -> x >= p && x < q) (zones_bounded z)

-- | <http://www8.garmin.com/manuals/Edge500_OwnersManual.pdf>
--
-- > let r = [(92,110),(111,128),(129,147),(148,165),(166,184)]
-- > in map zone_round (hr_zones_garmin 184) == r
hr_zones_garmin :: (Fractional a) => a -> [(a,a)]
hr_zones_garmin hr_max =
    let z = map (* hr_max) [0.5,0.6,0.7,0.8,0.9,1.0]
    in zip z (tail z)

-- | <http://www.cptips.com/hrmntr.htm>
--
-- > let r = [(92,119),(120,132),(135,147),(155,165),(168,184)]
-- > in map zone_round (hr_zones_ctips 184) == r
hr_zones_ctips :: (Fractional a) => a -> [(a,a)]
hr_zones_ctips hr_max =
    let a = [(0.5,0.65),(0.65,0.72),(0.73,0.80),(0.84,0.90),(0.91,1.0)]
        f (i,j) = (i * hr_max,j * hr_max)
    in map f a

-- > hr_zones_zoladz 184 == [(129,139),(139,149),(149,159),(159,169),(169,179)]
hr_zones_zoladz :: (Num a) => a -> [(a,a)]
hr_zones_zoladz hr_max =
    let adj = [50,40,30,20,10]
    in map (\x -> (hr_max - x - 5,hr_max - x + 5)) adj

-- | trainingbible.com/joesblog/2009/11/quick-guide-to-setting-zones.html
--
-- > let r = [(126,135),(136,148),(149,156),(157,165),(167,170),(173,177),(178,208)]
-- > in map zone_round (hr_zones_jf 167)
hr_zones_jf :: Fractional t => t -> [(t,t)]
hr_zones_jf lthr =
    let a = [(0.75,0.81),(0.81,0.89),(0.89,0.94),(0.94,0.99),(1.00,1.02)
            ,(1.03,1.06),(1.06,1.25)]
        f (i,j) = (i * lthr,j * lthr)
    in map f a

-- > map (hr_target_karvonen 54 184) [0.5,0.85] == [119.0,164.5]
hr_target_karvonen :: (Num a) => a -> a -> a -> a
hr_target_karvonen hr_rest hr_max n = ((hr_max - hr_rest) * n) + hr_rest

-- | <http://wattbike.com/au/guide/getting_started/heart_rate_and_power_training_zones>
--
-- > let r = [(0,111),(111,120),(121,138),(139,151),(152,164),(165,173),(174,185),(185,370)]
-- > in zones_bounded_hrm' 185 hr_zones_wb == r
hr_zones_wb :: Num n => [n]
hr_zones_wb = [0,60,65,75,82,89,94,100,200]

zone_name_wb :: [String]
zone_name_wb =
    ["0.Recovery","1.Basic","2.Basic"
    ,"3.Intensive","4.Intensive"
    ,"5.Maximal","6.Maximal","7.Supramaximal"]

-- > mapMaybe (hr_lookup_wb 185) [130,135 .. 185]
hr_lookup_wb :: (Fractional a, Ord a) => a -> a -> Maybe (a,Int, String)
hr_lookup_wb hrm hr =
    let p = (hr / hrm) * 100
    in case zone_lookup hr_zones_wb p of
         Just z -> Just (hr,z,zone_name_wb !! z)
         Nothing -> Nothing

-- | GCN HR numbers.
--
-- > let {hr = 167; r = [114,139,157,175]}
-- > in map (\x -> round (x * 0.01 * hr)) (mapMaybe snd hr_gcn) == r
hr_gcn :: Num n => [(String,Maybe n)]
hr_gcn =
    [("1. Active Recovery",Just 68)
    ,("2. Endurance",Just 83)
    ,("3. Tempo",Just 94)
    ,("4. Lactate Threshold",Just 105)
    ,("5. VO2 Max",Nothing)
    ,("6. Anaerobic Capacity",Nothing)
    ,("7. Neuromuscular",Nothing)
    ]

{-

> import Music.Theory.Array.MD
> let f nm (p,q) = [nm,show p,show q]
> let t = zipWith f zone_name_wb (zones_bounded_hrm' 185 hr_zones_wb)
> putStrLn$unlines$md_table Nothing t

-------------- --- ---
    0.Recovery   0 111
       1.Basic 111 120
       2.Basic 121 138
   3.Intensive 139 151
   4.Intensive 152 164
     5.Maximal 165 173
     6.Maximal 174 185
7.Supramaximal 185 370
-------------- --- ---

-}

-- | Calculate the average heart-rate after extracting an interval from it.
--
-- > let req (a,b) (c,d) = round a == round c && round b == round d
-- > hr_avg_extract (40,149) (5,137) `req` (35,151)
hr_avg_extract :: Fractional t => (t,t) -> (t,t) -> (t,t)
hr_avg_extract (p,q) (p',q') =
    let lhs = p * q
        rhs = p' * q'
        d = p - p'
    in (d,(lhs - rhs) / d)
