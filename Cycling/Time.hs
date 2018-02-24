-- | Time and duration related functions.
module Cycling.Time where

import qualified Data.Time as T {- time -}

import Music.Theory.Time.Notation {- hmt -}

-- * Duration

-- | Parse duration in @H:M:S@ form.
--
-- > parse_duration "21:44:30" == T.secondsToDiffTime 78270
parse_duration :: String -> T.DiffTime
parse_duration = T.utctDayTime . parse_iso8601_time

-- | Format duration in @H:M:S@ form, the duration must be less than
-- 24 hours.
--
-- > format_duration (parse_duration "21:44:30") == "21:44:30"
format_duration :: T.DiffTime -> String
format_duration t =
    let t' = T.UTCTime (T.ModifiedJulianDay 0) t
    in format_time_str "%H:%M:%S" t'

-- | Format fractional hours as @H:M:S@.
--
-- > format_fhour 21.75 == "21:45:00"
format_fhour :: FHOUR -> String
format_fhour = format_duration . fhour_to_difftime

-- | Add an 'Integer' number of days to a 'T.UTCTime'.
--
-- > toGregorian (utctDay (add_days 1 (parse_iso8601_date "2011-10-09"))) == (2011,10,10)
add_days :: Integer -> T.UTCTime -> T.UTCTime
add_days n (T.UTCTime d t) = T.UTCTime (T.addDays n d) t

-- | Variant of 'addGregorianMonthsClip'.
--
-- > toGregorian (utctDay (add_months 1 (parse_iso8601_date "2011-10-09"))) == (2011,11,09)
add_months :: Integer -> T.UTCTime -> T.UTCTime
add_months n (T.UTCTime d t) = T.UTCTime (T.addGregorianMonthsClip n d) t

