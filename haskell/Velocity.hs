module Velocity where

-- k = distance (km),h = hours,m = minutes,s = seconds,ms = milli-seconds
kph :: (Fractional a) => a -> (a, a, a, a) -> a
kph k (h,m,s,ms) =
    let t = h + (m / 60) + (s / (60 * 60)) + (ms / (60 * 60 * 100))
    in k / t

-- kph = kilometres per hour, mph = miles per hour
kph_to_mph :: (Fractional t) => t -> t
kph_to_mph = (*) 0.621371192

-- kph = kilometres per hour, mpm = minutes per metre
kph_to_mpm :: Double -> Double
kph_to_mpm x = (x * 1000) / 60

-- mpm = minutes per metre, kph = kilometres per hour
mpm_to_kph :: Double -> Double
mpm_to_kph x = (x * 60) / 1000

-- kph = kilometres per hour, mps = metres per second
kph_to_mps :: (Fractional a) => a -> a
kph_to_mps x = (x * 1000) / (60 * 60)
