module Power (velocity_std) where

import qualified Solve as S
import qualified Velocity as V

{-

Bicycling Science by Frank Whitt and David Wilson, p.142 (3rd ed.)

W = [Ka(V+Vw)^2+mg(s+Cr)+mEff(a)]V

W = power (watts)
Ka = drag factor (kg/m) (~= 0.19)
Vw = headwind (meters/sec)
m = mass of cyclist and bicycle (kg)
mE = effective mass (m + kinetic enegry of rotation of wheels)
g = gravitational accel (~=9.806 m/sec-sec at sea level)
s = slope or grade (%)
Cr = rolling resistance co-efficient (kg/m) (~= 0.003)
a = acceleration of the bicycle (meters/(sec)(sec))
V = speed of cyclist (meters/sec)

Kc = convergence parameter (0.5-2.0)
-}

power :: (Num a) => a -> a -> a -> a -> a -> a -> a -> a -> a -> a
power ka vw m mE g s cr a v = 
    ((ka * (v + vw) * (v + vw)) + (m * g * (s + cr)) + (mE * a)) * v

power_std :: (Fractional a) => a -> a -> a -> a
power_std m s v =
    let ka = 0.225
        vw = 0
        mE = m * 1.01
        g = 9.806
        cr = 0.003
        a = 0
    in power ka vw m mE g (s / 100) cr a (V.kph_to_mps v)

velocity_std :: (Fractional x, Ord x) => x -> x -> x -> x -> (x, x)
velocity_std t m s w = S.solve (power_std m s) t w 36 (4, EQ)
