-- | VAM (velocita ascensionale media)
--
-- * <http://en.wikipedia.org/wiki/Velocity_Ascended,_Metres_per_hour>
--
-- * <http://thecycleway.com/?p=748>
module Cycling.VAM where

-- | Synonym for 'Double'.
type R = Double

-- | Given vertical ascent (in metres) and time (in minutes) and
-- average gradient (as percentage) calculate the /VAM/.
velocita_ascensionale_media :: R -> R -> R
velocita_ascensionale_media va t = (va * 60) / t

-- | Synonym for 'velocita_ascensionale_media'.
--
-- > vam 600 20 == 1800
vam :: R -> R -> R
vam = velocita_ascensionale_media

-- | Account for gradient.
gradient_factor :: R -> R
gradient_factor x = 2 + (x / 10)

-- | Account for altitude.  \"I think it is reasonable to evaluate an
-- average reduction in VAM values by approximately 3% every 500m of
-- elevation\" (/MF/)
--
-- * <http://www.53x12.com/do/show?page=article&id=74>
altitude_factor :: R -> R
altitude_factor a = 1 - ((a / 500) * 0.03)

-- | Convert from 'vam' to power approximation (in watts per kilogram)
-- taking into account the /alititude/ and /gradient/.
--
-- > map (vam_to_power 1800 0) [5,10] == [7.2,6]
vam_to_power :: R -> R -> R -> R
vam_to_power x a gr =
    let n = x / (gradient_factor gr * 100)
    in n * altitude_factor a
