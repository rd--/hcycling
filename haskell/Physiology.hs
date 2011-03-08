module Physiology where

type HR_MAX = Double->Double

hr_max_haskell_and_fox :: HR_MAX
hr_max_haskell_and_fox age = 220 - age

hr_max_robergs_and_landwehr :: HR_MAX
hr_max_robergs_and_landwehr age = 205.8 - (0.685 * age)

hr_max_londeree_and_moeschberger :: HR_MAX
hr_max_londeree_and_moeschberger age = 206.3 - (0.711 * age)

hr_max_miller_et_al :: HR_MAX
hr_max_miller_et_al age = 217 - (0.85 * age)

hr_max_tanaka :: HR_MAX
hr_max_tanaka age = 208 - (0.7 * age)

hr_max_oakland_nonlinear :: HR_MAX
hr_max_oakland_nonlinear age = 191.5 - (0.007 * age * age)

hr_max_lund :: HR_MAX
hr_max_lund age = 203.7 / (1 + exp (0.033 * (age - 104.3)))

hr_max_all :: [HR_MAX]
hr_max_all =
    [hr_max_haskell_and_fox
    ,hr_max_robergs_and_landwehr
    ,hr_max_londeree_and_moeschberger
    ,hr_max_miller_et_al
    ,hr_max_tanaka
    ,hr_max_oakland_nonlinear
    ,hr_max_lund]

-- http://www8.garmin.com/manuals/Edge500_OwnersManual.pdf
hr_zones_garmin :: (Fractional a) => a -> [(a,a)]
hr_zones_garmin hr_max =
    let z = map (* hr_max) [0.5,0.6,0.7,0.8,0.9,1.0]
    in zip z (tail z)

-- http://www.cptips.com/hrmntr.htm
hr_zones_ctips :: (Fractional a) => a -> [(a,a)]
hr_zones_ctips hr_max =
    let a = [(0.5,0.65),(0.65,0.72),(0.73,0.80),(0.84,0.90),(0.91,1.0)]
        f (i,j) = (i * hr_max,j * hr_max)
    in map f a

hr_zones_zoladz :: (Num a) => a -> [(a, a)]
hr_zones_zoladz hr_max =
    let adj = [50,40,30,20,10]
    in map (\x -> (hr_max - x - 5,hr_max - x + 5)) adj

hr_target_karvonen :: (Num a) => a -> a -> a -> a
hr_target_karvonen hr_rest hr_max n = ((hr_max - hr_rest) * n) + hr_rest

{-
let n = 36 in map (\f -> f n) hr_max_all
hr_zones_garmin 184
hr_zones_ctips 184
hr_zones_zoladz 184
map (hr_target_karvonen 54 184) [0.5,0.85]
-}

