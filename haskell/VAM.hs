module VAM where

{-
http://www.53x12.com/do/show?page=article&id=74
http://en.wikipedia.org/wiki/Velocity_Ascended,_Metres_per_hour
http://thecycleway.com/?p=748
-}

type R = Double

gradient_factor :: R -> R
gradient_factor x = 2 + (x / 10)

-- va = vertical ascent in metres
-- t = time in minutes
-- gr = average gradient in percentage
velocita_ascensionale_media :: R -> R -> R
velocita_ascensionale_media va t = (va * 60) / t

vam :: R -> R -> R
vam = velocita_ascensionale_media

-- I think it is reasonable to evaluate an average reduction in VAM
-- values by approximately 3% every 500m of elevation,
altitude_factor :: R -> R
altitude_factor a = 1 - ((a / 500) * 0.03)

vam_to_power :: R -> R -> R -> R
vam_to_power x a gr =
    let n = x / (gradient_factor gr * 100)
    in n * altitude_factor a

{-
vam 600 20
map (vam_to_power 1800 0) [6..11]
-}
