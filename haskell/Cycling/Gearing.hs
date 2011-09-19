module Cycling.Gearing ( Gear(..)
                       , Tyre(..), read_iso_tyre
                       , cadence
                       , velocity
                       , gear_metres, gear_inches ) where

import qualified Data.List.Split as S
import qualified Cycling.Velocity as V

-- ISO tyre specification (millimetres)
data Tyre = Tyre { tyre_section :: Int
                 , bead_diameter :: Int }

instance Show Tyre where
    show (Tyre s b) = show s ++ "-" ++ show b

read_iso_tyre :: String -> Tyre
read_iso_tyre x =
    let [s,b] = S.sepBy "-" x
    in Tyre (read s) (read b)

instance Read Tyre where
    readsPrec _ x = [(read_iso_tyre x,"")]

iso_m :: Tyre -> (Double, Double)
iso_m (Tyre s b) = (fromIntegral s / 1000, fromIntegral b / 1000)

-- in metres
rollout :: Tyre -> Double
rollout t =
    let (s, b) = iso_m t
    in (s * 2 + b) * pi

data Gear = Gear { chainwheel :: Int
                 , sprocket :: Int }

instance Show Gear where
    show (Gear c s) = show c ++ "/" ++ show s

ratio :: Gear -> Double
ratio (Gear c s) = fromIntegral c / fromIntegral s

-- t = tyre (Tyre), g = gear (Gear), v = velocity (kph)
cadence :: Tyre -> Gear -> Double -> Double
cadence t g v =
    let v' = V.kph_to_mpm v
    in v' / (rollout t * ratio g)

-- t = tyre (Tyre), g = gear (Gear), c = cadence (rpm)
velocity :: Tyre -> Gear -> Double -> Double
velocity t g c =
    let mpm = rollout t * ratio g * c
    in V.mpm_to_kph mpm

gear_metres :: Tyre -> Gear -> Double
gear_metres t g = rollout t * ratio g

{-
metres_to_inches :: Double -> Double
metres_to_inches = (* 39.3700787)
-}

gear_inches :: Tyre -> Gear -> Double
gear_inches t g =
    let n = 0.0254 * pi
    in (gear_metres t g) / n
