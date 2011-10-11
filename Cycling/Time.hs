-- | Time and duration related functions.
module Cycling.Time where

import qualified Data.List.Split as S
import Data.Time {- time -}
import qualified Foreign.C.Math.Double as M {- cmath -}
import System.Locale {- old-locale -}
import Text.Printf

-- | Duration stored as /hours/, /minutes/, /seconds/ and /milliseconds/.
data Duration = Duration {hours :: Integer
                         ,minutes :: Integer
                         ,seconds :: Integer
                         ,milliseconds :: Integer}
                deriving (Eq)

-- | Convert fractional /seconds/ to integral /(seconds,milliseconds)/.
--
-- > s_sms 1.75 == (1,750)
s_sms :: (RealFrac n,Integral i) => n -> (i,i)
s_sms s =
    let s' = floor s
        ms = round ((s - fromIntegral s') * 1000)
    in (s',ms)

-- | Inverse of 's_sms'.
--
-- > sms_s (1,750) == 1.75
sms_s :: (Integral i) => (i,i) -> Double
sms_s (s,ms) = fromIntegral s + fromIntegral ms / 1000

-- | 'Read' function for 'Duration' tuple.
read_duration_tuple :: String -> (Integer,Integer,Integer,Integer)
read_duration_tuple x =
    let f :: (Integer,Integer,Double) -> (Integer,Integer,Integer,Integer)
        f (h,m,s) = let (s',ms) = s_sms s in (h,m,s',ms)
    in case S.splitOneOf ":" x of
        [h,m,s] -> f (read h,read m,read s)
        [m,s] -> f (0,read m,read s)
        [s] -> f (0,0,read s)
        _ -> error "read_duration_tuple"

-- | 'Read' function for 'Duration'.  Allows either @H:M:S.MS@ or
-- @M:S.MS@ or @S.MS@.
--
-- > read_duration "01:35:05.250" == Duration 1 35 5 250
-- > read_duration    "35:05.250" == Duration 0 35 5 250
-- > read_duration       "05.250" == Duration 0 0 5 250
read_duration :: String -> Duration
read_duration = tuple_to_duration id . read_duration_tuple

instance Read Duration where
    readsPrec _ x = [(read_duration x,"")]

-- | 'Show' function for 'Duration'.
--
-- > show_duration (Duration 1 35 5 250) == "01:35:05.250"
-- > show (Duration 1 15 0 000) == "01:15:00.000"
show_duration :: Duration -> String
show_duration (Duration h m s ms) =
    let f :: Int -> String
        f = printf "%02d"
        g = f . fromIntegral
        s' = sms_s (s,ms)
    in concat [g h,":",g m,":",printf "%06.3f" s']

instance Show Duration where
    show = show_duration

-- | Extract 'Duration' tuple applying filter function at each element
--
-- > duration_tuple id (Duration 1 35 5 250) == (1,35,5,250)
duration_to_tuple :: (Integer -> a) -> Duration -> (a,a,a,a)
duration_to_tuple f (Duration h m s ms) = (f h,f m,f s,f ms)

-- | Inverse of 'duration_to_tuple'.
tuple_to_duration :: (a -> Integer) -> (a,a,a,a) -> Duration
tuple_to_duration f (h,m,s,ms) = Duration (f h) (f m) (f s) (f ms)

-- * Clock time related functions

-- | Parse date in @Y-m-d@ form.
--
-- > toGregorian (utctDay (parse_date "2011-10-09")) == (2011,10,9)
parse_date :: String -> UTCTime
parse_date = readTime defaultTimeLocale "%F"

-- | Format date in @Y-m-d@ form.
--
-- > format_date (parse_date "2011-10-09") == "2011-10-09"
format_date :: UTCTime -> String
format_date = formatTime defaultTimeLocale "%F"

-- | Parse date in @Y-m-d@ and time in @H:M@ forms.
--
-- > utctDayTime (parse_date_time ("2011-10-09","21:44")) == secondsToDiffTime 78240
parse_date_time :: (String,String) -> UTCTime
parse_date_time (d,t) = readTime defaultTimeLocale "%F%H:%M" (d++t)

-- | Format time in @H:M@ form.
--
-- > format_time (parse_date_time ("2011-10-09","21:44")) == "21:44"
format_time :: UTCTime -> String
format_time = formatTime defaultTimeLocale "%H:%M"

-- | Format date in @Y-m-d@ and time in @H:M@ forms.
--
-- > format_date_time (parse_date_time ("2011-10-09","21:44")) == ("2011-10-09","21:44")
format_date_time :: UTCTime -> (String,String)
format_date_time t = (format_date t,format_time t)

-- | Parse duration in @H:M'S@ form.
--
-- > parse_duration "21:44'30" == secondsToDiffTime 78270
parse_duration :: String -> DiffTime
parse_duration = utctDayTime . readTime defaultTimeLocale "%H:%M'%S"

-- | Format duration in @H:M'S@ form, the duration must be less than
-- 24 hours.
--
-- > format_duration (parse_duration "21:44'30") == "21:44'30"
format_duration :: DiffTime -> String
format_duration t =
    let t' = UTCTime (ModifiedJulianDay 0) t
    in formatTime defaultTimeLocale "%H:%M'%S" t'

-- | 'DiffTime' in fractional seconds.
--
-- > diff_time_seconds (parse_duration "21:44'30") == 78270
diff_time_seconds :: DiffTime -> Double
diff_time_seconds = fromRational . toRational

-- | 'DiffTime' in fractional minutes.
--
-- > diff_time_minutes (parse_duration "21:44'30") == 1304.5
diff_time_minutes :: DiffTime -> Double
diff_time_minutes = (/ 60) . diff_time_seconds

-- | 'DiffTime' in fractional hours.
--
-- > diff_time_hours (parse_duration "21:45'00") == 21.75
diff_time_hours :: DiffTime -> Double
diff_time_hours = (/ 60) . diff_time_minutes

-- | Format fractional hours as @H:M'S@.
--
-- > format_hours 21.75 == "21:45'00"
format_hours :: Double -> String
format_hours h =
    let m = (h - M.floor h) * 60
        s = (m - M.floor m) * 60
        f :: Double -> Int
        f = floor
    in printf "%d:%02d'%02d" (f h) (f m) (f s)

-- | Add an 'Integer' number of days to a 'UTCTime'.
--
-- > toGregorian (utctDay (add_days 1 (parse_date "2011-10-09"))) == (2011,10,10)
add_days :: Integer -> UTCTime -> UTCTime
add_days n (UTCTime d t) = UTCTime (addDays n d) t

-- | Time in fractional days.
--
-- > round (time_days (parse_date_time ("2011-10-09","09:00"))) == 55843
-- > round (time_days (parse_date_time ("2011-10-09","21:00"))) == 55844
time_days :: UTCTime -> Double
time_days t =
    let d = utctDay t
        d' = fromIntegral (toModifiedJulianDay d)
        s = utctDayTime t
        s_max = 86401
    in d' + (fromRational (toRational s) / s_max)
