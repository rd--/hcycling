module Cycling.Power where

import qualified Cycling.Solve as S
import qualified Cycling.Velocity as V

-- | /Bicycling Science/ by Frank Whitt and David Wilson, p.142 (3rd ed.)
-- The implemented formula is
--
-- > W = [Ka(V+Vw)^2+mg(s+Cr)+mEff(a)]V
--
-- where
--
-- @
-- /W/ = power (watts)
-- /Ka/ = drag factor (kg/m) (~= 0.19)
-- /Vw/ = headwind (meters/sec)
-- /m/ = mass of cyclist and bicycle (kg)
-- /mE/ = effective mass (m + kinetic enegry of rotation of wheels)
-- /g/ = gravitational accel (~=9.806 m/sec-sec at sea level)
-- /s/ = slope or grade (%)
-- /Cr/ = rolling resistance co-efficient (kg/m) (~= 0.003)
-- /a/ = acceleration of the bicycle (meters/(sec)(sec))
-- /V/ = speed of cyclist (meters/sec)
-- /Kc/ = convergence parameter (0.5-2.0)
-- @
power :: (Num a) => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
power ka vw m mE g s cr a v =
    ((ka * (v + vw) * (v + vw)) + (m * g * (s + cr)) + (mE * a)) * v

-- | Variant of 'power' with standard values of /Ka/ (0.225) and /vw/
-- (0) and /mE/ (1.01 /m/) and /g/ (9.806) and /cr/ (0.003) and /a/
-- (0).
--
-- > round (power_std 70 8 20) == 355
power_std :: (Fractional a) => a -> a -> a -> a
power_std m s v =
    let ka = 0.225
        vw = 0
        mE = m * 1.01
        g = 9.806
        cr = 0.003
        a = 0
    in power ka vw m mE g (s / 100) cr a (V.kph_to_mps v)

-- | 'S.solve' 'power_std' equation for velocity with given /tolerance/.
--
-- > round (fst (velocity_std 0.05 70 8 355)) == 20
velocity_std :: (Fractional x, Ord x) => x -> x -> x -> x -> (x, x)
velocity_std t m s w = S.solve (S.Solve (power_std m s) t w 36 4)
