module Cycling.Gearing where

import qualified Data.List.Split as S
import qualified Cycling.Velocity as V

-- | ISO tyre specification (millimetres)
data Tyre = Tyre {tyre_section :: Int
                 ,bead_diameter :: Int}

-- | 'Show' function for 'Tyre'.
show_iso_tyre :: Tyre -> String
show_iso_tyre (Tyre s b) = show s ++ "-" ++ show b

instance Show Tyre where
    show = show_iso_tyre

-- | 'Read' function for 'Tyre'.
read_iso_tyre :: String -> Tyre
read_iso_tyre x =
    let [s,b] = S.sepBy "-" x
    in Tyre (read s) (read b)

instance Read Tyre where
    readsPrec _ x = [(read_iso_tyre x,"")]

-- | Translate 'Tyre' into metre base duple
--
-- > iso_m (Tyre 23 622) == (23*1e-3,622*1e-3)
iso_m :: Tyre -> (Double,Double)
iso_m (Tyre s b) = (fromIntegral s / 1000,fromIntegral b / 1000)

-- | 'Tyre' rollout in metres.
--
-- > rollout (Tyre 23 622) == 2.098583892597982
rollout :: Tyre -> Double
rollout t =
    let (s,b) = iso_m t
    in (s * 2 + b) * pi

-- | Gear ratio.
data Gear = Gear {chainwheel :: Int
                 ,sprocket :: Int}

-- | 'Show' function for 'Gear'
show_gear :: Gear -> String
show_gear (Gear c s) = show c ++ "/" ++ show s

instance Show Gear where
    show = show_gear

-- | 'Gear' ratio.
--
-- > ratio (Gear 53 15) == 53/15
ratio :: Fractional n => Gear -> n
ratio (Gear c s) = fromIntegral c / fromIntegral s

type Velocity = Double
type Cadence = Double

-- | Given 'Tyre', 'Gear' and 'Velocity' (in kph) calculate 'Cadence'.
--
-- > cadence (Tyre 23 622) (Gear 53 16) 45 == 107.8894654334394
cadence :: Tyre -> Gear -> Velocity -> Cadence
cadence t g v =
    let v' = V.kph_to_mpm v
    in v' / (rollout t * ratio g)

-- | Given 'Tyre', 'Gear' and 'Cadence' (in rpm) calculate 'Velocity'.
--
-- > velocity (Tyre 23 622) (Gear 53 16) 108 == 45.04610325461568
velocity :: Tyre -> Gear -> Cadence -> Velocity
velocity t g c =
    let mpm = rollout t * ratio g * c
    in V.mpm_to_kph mpm

-- | Given 'Tyre' and 'Gear' calculate /gear/ in metres.
gear_metres :: Tyre -> Gear -> Double
gear_metres t g = rollout t * ratio g

{-
metres_to_inches :: Double -> Double
metres_to_inches = (* 39.3700787)
-}

-- | Given 'Tyre' and 'Gear' calculate /gear/ in inches.
--
-- > gear_inches (Tyre 23 622) (Gear 48 15) == 84.15748031496064
-- > gear_inches (Tyre 28 630) (Gear 44 16) == 74.27165354330708
gear_inches :: Tyre -> Gear -> Double
gear_inches t g =
    let n = 0.0254 * pi
    in gear_metres t g / n
