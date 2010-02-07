module Gearing ( Gear(..)
               , Tyre(..)
               , cadence
               , velocity ) where

-- ISO tyre specification (millimetres)
data Tyre = Tyre { tyre_section :: Int
     	         , bead_diameter :: Int }

instance Show Tyre where
    show (Tyre s b) = show s ++ "-" ++ show b

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

-- kph = kilometres per hour, mpm = minutes per metre, 
kph_mpm :: Double -> Double
kph_mpm x = (x * 1000) / 60

-- t = tyre (Tyre), g = gear (Gear), v = velocity (kph)
cadence :: Tyre -> Gear -> Double -> Double
cadence t g v =
    let v' = kph_mpm v
    in v' / (rollout t * ratio g)

-- mpm = minutes per metre, kph = kilometres per hour
mpm_kph :: Double -> Double
mpm_kph x = (x * 60) / 1000

-- t = tyre (Tyre), g = gear (Gear), c = cadence (rpm)
velocity :: Tyre -> Gear -> Double -> Double
velocity t g c =
    let mpm = rollout t * ratio g * c
    in mpm_kph mpm
