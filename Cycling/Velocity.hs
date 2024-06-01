-- | Velocity and distance (unit) related functions.
module Cycling.Velocity where

import Music.Theory.Time.Duration {- hmt-base -}

{- | Conversion factor from miles to kilometers.

>>> mile / 1.609344
1.0
-}
mile :: Fractional t => t
mile = 1.609344

{- | Convert from kilometres per hour to miles per hour.

>>> round (kph_to_mph 60)
37
-}
kph_to_mph :: (Fractional t) => t -> t
kph_to_mph = (*) (recip mile)

{- | Convert from miles per hour to kilometers per hour.

>>> round (mph_to_kph 37)
60
-}
mph_to_kph :: (Fractional t) => t -> t
mph_to_kph = (*) mile

{- | Convert from kilometres per hour to metres per minute.

>>> kph_to_mpm 45
750.0
-}
kph_to_mpm :: Double -> Double
kph_to_mpm x = (x * 1000) / 60

{- | Inverse of 'kph_to_mpm'.

>>> mpm_to_kph 750
45.0
-}
mpm_to_kph :: Double -> Double
mpm_to_kph x = (x * 60) / 1000

{- | Convert from kilometres per hour to metres per second.

>>> kph_to_mps 45
12.5
-}
kph_to_mps :: (Fractional a) => a -> a
kph_to_mps x = (x * 1000) / (60 * 60)

{- | Convert from metres per second (mps) to kilometres per hour (kph).

>>> mps_to_kph 12.5
45.0
-}
mps_to_kph :: Fractional a => a -> a
mps_to_kph n = (n * 60 * 60) / 1000

-- | Conversion factor from nautical miles to kilometers.
nautical_mile :: Fractional t => t
nautical_mile = 1.852

-- | Convestion factor from feet to meters.
foot :: Fractional t => t
foot = 0.3048

-- | Convert from feet to meters.
feet_to_metres :: (Fractional t) => t -> t
feet_to_metres = (*) foot

{- | Given a /distance/ (in km) and a 'Duration' (in hours, minutes,
seconds and milli-seconds) calculate average velocity (in kph),

>>> map (kph 0.25) [Duration 0 0 20 0,Duration 0 0 15 0]
[45.0,60.0]
-}
kph :: (Fractional a) => a -> Duration -> a
kph k d = k / duration_to_hours d
