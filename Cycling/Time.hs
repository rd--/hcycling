-- | Time and duration related functions.
module Cycling.Time where

import Data.List.Split {- time -}
import Data.Time {- time -}

-- * Clock time related functions

parse_time_str :: String -> String -> UTCTime
parse_time_str = parseTimeOrError True defaultTimeLocale

format_time_str :: String -> UTCTime -> String
format_time_str = formatTime defaultTimeLocale

-- | Parse date in ISO-8601 (@Y-m-d@) form.
--
-- > toGregorian (utctDay (parse_iso8601_date "2011-10-09")) == (2011,10,9)
parse_iso8601_date :: String -> UTCTime
parse_iso8601_date = parse_time_str "%F"

-- | Format date in ISO-8601 (@Y-m-d@) form.
--
-- > format_iso8601_date (parse_iso8601_date "2011-10-09") == "2011-10-09"
format_iso8601_date :: UTCTime -> String
format_iso8601_date = format_time_str "%F"

-- > format_iso8601_week (parse_iso8601_date "2011-10-09") == "2011-W40"
format_iso8601_week :: UTCTime -> String
format_iso8601_week = format_time_str "%G-W%V"

type WEEK = Int

-- > time_to_week (parse_iso8601_date "2011-10-09") == 40
time_to_week :: UTCTime -> WEEK
time_to_week = read . format_time_str "%V"

parse_iso8601_time :: String -> UTCTime
parse_iso8601_time = parse_time_str "%H:%M:%S"

-- | Format time in @H:M:S@ form.
--
-- > format_iso8601_time (parse_iso8601_date_time "2011-10-09T21:44:00") == "21:44:00"
format_iso8601_time :: UTCTime -> String
format_iso8601_time = format_time_str "%H:%M:%S"

-- | Parse date in @Y-m-d@ and time in @H:M:%S@ forms.
--
-- > utctDayTime (parse_iso8601_date_time "2011-10-09T21:44:00") == secondsToDiffTime 78240
parse_iso8601_date_time :: String -> UTCTime
parse_iso8601_date_time = parse_time_str "%FT%H:%M:%S"

-- | Format date in @Y-m-d@ and time in @H:M:S@ forms.
--
-- > let t = parse_iso8601_date_time "2011-10-09T21:44:00"
-- > in format_iso8601_date_time t == "2011-10-09T21:44:00"
format_iso8601_date_time :: UTCTime -> String
format_iso8601_date_time = format_time_str "%FT%H:%M:%S"

-- | Parse duration in @H:M:S@ form.
--
-- > parse_duration "21:44:30" == secondsToDiffTime 78270
parse_duration :: String -> DiffTime
parse_duration = utctDayTime . parse_iso8601_time

-- | Format duration in @H:M:S@ form, the duration must be less than
-- 24 hours.
--
-- > format_duration (parse_duration "21:44:30") == "21:44:30"
format_duration :: DiffTime -> String
format_duration t =
    let t' = UTCTime (ModifiedJulianDay 0) t
    in format_time_str "%H:%M:%S" t'

-- | 'DiffTime' in fractional seconds.
--
-- > difftime_to_fsec (parse_duration "21:44:30") == 78270
difftime_to_fsec :: DiffTime -> Double
difftime_to_fsec = fromRational . toRational

-- | 'DiffTime' in fractional minutes.
--
-- > difftime_to_fmin (parse_duration "21:44:30") == 1304.5
difftime_to_fmin :: DiffTime -> Double
difftime_to_fmin = (/ 60) . difftime_to_fsec

-- | 'DiffTime' in fractional hours.
--
-- > difftime_to_fhr (parse_duration "21:45:00") == 21.75
difftime_to_fhr :: DiffTime -> Double
difftime_to_fhr = (/ 60) . difftime_to_fmin

floor' :: Double -> Double
floor' = fromInteger . floor

-- | Fractional hour to (hours,minutes,seconds).
--
-- > fhr_to_hrminsec 21.75 == (21,45,0)
fhr_to_hrminsec :: Double -> (Int,Int,Int)
fhr_to_hrminsec h =
    let m = (h - floor' h) * 60
        s = (m - floor' m) * 60
    in (floor h,floor m,round s)

-- > fhr_to_fsec 21.75 == 78300.0
fhr_to_fsec :: Num n => n -> n
fhr_to_fsec = (*) (60 * 60)

-- | Fractional hour.
type FHR = Double

-- > fsec_to_picoseconds 78240.05
fsec_to_picoseconds :: FHR -> Integer
fsec_to_picoseconds s = floor (s * (10 ** 12))

-- | Fractional seconds.
type FSEC = Double

fsec_to_difftime :: FSEC -> DiffTime
fsec_to_difftime = picosecondsToDiffTime . fsec_to_picoseconds

fhr_to_difftime :: FHR -> DiffTime
fhr_to_difftime = fsec_to_difftime . fhr_to_fsec

-- | Format fractional hours as @H:M:S@.
--
-- > format_fhr 21.75 == "21:45:00"
format_fhr :: FHR -> String
format_fhr = format_duration . fhr_to_difftime

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

-- | Fractional days.
type FDAY = Double

-- | Time in fractional days.
--
-- > round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T09:00:00")) == 55843
-- > round (utctime_to_fday (parse_iso8601_date_time "2011-10-09T21:00:00")) == 55844
utctime_to_fday :: UTCTime -> FDAY
utctime_to_fday t =
    let d = utctDay t
        d' = fromIntegral (toModifiedJulianDay d)
        s = utctDayTime t
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
