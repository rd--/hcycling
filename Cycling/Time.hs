-- | Time and duration related functions.
module Cycling.Time where

import Data.Time {- time -}

import qualified Music.Theory.Time.Notation as T {- hmt -}

-- * Duration

-- | Parse duration in @H:M:S@ form.
--
-- > parse_duration "21:44:30" == secondsToDiffTime 78270
parse_duration :: String -> DiffTime
parse_duration = utctDayTime . T.parse_iso8601_time

-- | Format duration in @H:M:S@ form, the duration must be less than
-- 24 hours.
--
-- > format_duration (parse_duration "21:44:30") == "21:44:30"
format_duration :: DiffTime -> String
format_duration t =
    let t' = UTCTime (ModifiedJulianDay 0) t
    in T.format_time_str "%H:%M:%S" t'

-- | Format fractional hours as @H:M:S@.
--
-- > format_fhour 21.75 == "21:45:00"
format_fhour :: T.FHOUR -> String
format_fhour = format_duration . T.fhour_to_difftime

-- | Add an 'Integer' number of days to a 'UTCTime'.
--
-- > toGregorian (utctDay (add_days 1 (parse_iso8601_date "2011-10-09"))) == (2011,10,10)
add_days :: Integer -> UTCTime -> UTCTime
add_days n (UTCTime d t) = UTCTime (addDays n d) t

-- | Variant of 'addGregorianMonthsClip'.
--
-- > toGregorian (utctDay (add_months 1 (parse_iso8601_date "2011-10-09"))) == (2011,11,09)
add_months :: Integer -> UTCTime -> UTCTime
add_months n (UTCTime d t) = UTCTime (addGregorianMonthsClip n d) t

