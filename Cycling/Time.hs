-- | Time and duration related functions.
module Cycling.Time where

import Data.List.Split {- time -}
import qualified Data.Time as T {- time -}

-- * Util

floor' :: Double -> Double
floor' = fromInteger . floor

-- * Clock time related functions

parse_time_str :: String -> String -> T.UTCTime
parse_time_str = T.parseTimeOrError True T.defaultTimeLocale

format_time_str :: String -> T.UTCTime -> String
format_time_str = T.formatTime T.defaultTimeLocale

-- | Week number (1-52)
type WEEK = Int

-- | Week that /t/ lies in.
--
-- > map (time_to_week . parse_iso8601_date) ["2017-01-01","2011-10-09"] == [52,40]
time_to_week :: T.UTCTime -> WEEK
time_to_week = read . format_time_str "%V"

-- * ISO-8601

-- | Parse date in ISO-8601 (@Y-m-d@) form.
--
-- > T.toGregorian (T.utctDay (parse_iso8601_date "2011-10-09")) == (2011,10,9)
parse_iso8601_date :: String -> T.UTCTime
parse_iso8601_date = parse_time_str "%F"

-- | Format date in ISO-8601 (@Y-m-d@) form.
--
-- > format_iso8601_date (parse_iso8601_date "2011-10-09") == "2011-10-09"
format_iso8601_date :: T.UTCTime -> String
format_iso8601_date = format_time_str "%F"

{- | Format date in ISO-8601 (@Y-W@) form.

> let r = ["2016-W52","2011-W40"]
> in map (format_iso8601_week . parse_iso8601_date) ["2017-01-01","2011-10-09"] == r

-}
format_iso8601_week :: T.UTCTime -> String
format_iso8601_week = format_time_str "%G-W%V"

-- | Parse ISO-8601 @H:M:S@ time.
--
-- > format_iso8601_time (parse_iso8601_time "21:44:00") == "21:44:00"
parse_iso8601_time :: String -> T.UTCTime
parse_iso8601_time = parse_time_str "%H:%M:%S"

-- | Format time in @H:M:S@ form.
--
-- > format_iso8601_time (parse_iso8601_date_time "2011-10-09T21:44:00") == "21:44:00"
format_iso8601_time :: T.UTCTime -> String
format_iso8601_time = format_time_str "%H:%M:%S"

-- | Parse date in @Y-m-d@ and time in @H:M:%S@ forms.
--
-- > T.utctDayTime (parse_iso8601_date_time "2011-10-09T21:44:00") == T.secondsToDiffTime 78240
parse_iso8601_date_time :: String -> T.UTCTime
parse_iso8601_date_time = parse_time_str "%FT%H:%M:%S"

-- | Format date in @Y-m-d@ and time in @H:M:S@ forms.
--
-- > let t = parse_iso8601_date_time "2011-10-09T21:44:00"
-- > in format_iso8601_date_time t == "2011-10-09T21:44:00"
format_iso8601_date_time :: T.UTCTime -> String
format_iso8601_date_time = format_time_str "%FT%H:%M:%S"

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

-- | 'T.DiffTime' in fractional seconds.
--
-- > difftime_to_fsec (parse_duration "21:44:30") == 78270
difftime_to_fsec :: T.DiffTime -> Double
difftime_to_fsec = fromRational . toRational

-- | 'T.DiffTime' in fractional minutes.
--
-- > difftime_to_fmin (parse_duration "21:44:30") == 1304.5
difftime_to_fmin :: T.DiffTime -> Double
difftime_to_fmin = (/ 60) . difftime_to_fsec

-- | Fractional hour.
type FHOUR = Double

-- | 'T.DiffTime' in fractional hours.
--
-- > difftime_to_fhour (parse_duration "21:45:00") == 21.75
difftime_to_fhour :: T.DiffTime -> FHOUR
difftime_to_fhour = (/ 60) . difftime_to_fmin

-- | Fractional hour to (hours,minutes,seconds).
--
-- > fhour_to_hourminsec 21.75 == (21,45,0)
fhour_to_hourminsec :: FHOUR -> (Int,Int,Int)
fhour_to_hourminsec h =
    let m = (h - floor' h) * 60
        s = (m - floor' m) * 60
    in (floor h,floor m,round s)

-- | Fractional seconds.
type FSEC = Double

-- | Fractional hour to seconds.
--
-- > fhour_to_fsec 21.75 == 78300.0
fhour_to_fsec :: FHOUR -> FSEC
fhour_to_fsec = (*) (60 * 60)

-- > fsec_to_picoseconds 78240.05
fsec_to_picoseconds :: FSEC -> Integer
fsec_to_picoseconds s = floor (s * (10 ** 12))

fsec_to_difftime :: FSEC -> T.DiffTime
fsec_to_difftime = T.picosecondsToDiffTime . fsec_to_picoseconds

fhour_to_difftime :: FHOUR -> T.DiffTime
fhour_to_difftime = fsec_to_difftime . fhour_to_fsec

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

-- | Fractional days.
type FDAY = Double

-- | Time in fractional days.
--
-- > round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T09:00:00")) == 55843
-- > round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T21:00:00")) == 55844
utctime_to_fday :: T.UTCTime -> FDAY
utctime_to_fday t =
    let d = T.utctDay t
        d' = fromIntegral (T.toModifiedJulianDay d)
        s = T.utctDayTime t
        s_max = 86401
    in d' + (fromRational (toRational s) / s_max)

-- * DHMS

type DAY = Int
type HOUR = Int
type MIN = Int
type SEC = Int

-- | (days,hours,minutes,seconds)
type DHMS = (DAY,HOUR,MIN,SEC)

-- | Convert seconds into (days,hours,minutes,seconds).
--
-- > sec_to_dhms 1475469 == (17,1,51,9)
sec_to_dhms' :: Integral n => n -> (n,n,n,n)
sec_to_dhms' n =
    let (d,h') = n `divMod` (24 * 60 * 60)
        (h,m') = h' `divMod` (60 * 60)
        (m,s) = m' `divMod` 60
    in (d,h,m,s)

sec_to_dhms :: SEC -> DHMS
sec_to_dhms = sec_to_dhms'

-- | Inverse of 'seconds_to_dhms'.
--
-- > dhms_to_sec (17,1,51,9) == 1475469
dhms_to_sec :: Num n => (n,n,n,n) -> n
dhms_to_sec (d,h,m,s) = sum [d * 24 * 60 * 60,h * 60 * 60,m * 60,s]

parse_dhms' :: (Integral n,Read n) => String -> (n,n,n,n)
parse_dhms' =
    let sep_elem = split . keepDelimsR . oneOf
        sep_last x = let e:x' = reverse x in (reverse x',e)
        p x = case sep_last x of
                (n,'d') -> read n * 24 * 60 * 60
                (n,'h') -> read n * 60 * 60
                (n,'m') -> read n * 60
                (n,'s') -> read n
                _ -> error "parse_dhms"
    in sec_to_dhms' . sum . map p . filter (not . null) . sep_elem "dhms"

-- | Parse DHMS text.  All parts are optional, order is not
-- significant, multiple entries are allowed.
--
-- > parse_dhms "17d1h51m9s" == (17,1,51,9)
-- > parse_dhms "1s3d" == (3,0,0,1)
-- > parse_dhms "1h1h" == (0,2,0,0)
parse_dhms :: String -> DHMS
parse_dhms = parse_dhms'
