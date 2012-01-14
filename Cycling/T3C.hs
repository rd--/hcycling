-- | Suunto T3C HR monitor related functions
module Cycling.T3C where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Time {- time -}
import Text.CSV {- csv -}

import Cycling.Analysis
import qualified Cycling.Interval as I
import Cycling.Time

-- * T3C data type

-- | T3C session data.
data T3C = T3C {date_time :: UTCTime
               ,duration :: DiffTime
               ,training_effect :: R
               ,hr_average :: R
               ,hr_maximum :: R
               ,energy :: R
               ,notes :: String}
           deriving (Eq,Show)

-- | Real valued duration (hours).
duration_h :: T3C -> R
duration_h = diff_time_hours . duration

-- | Real valued time-stamp (days).
time_stamp :: T3C -> R
time_stamp = time_days . date_time

-- | Parse list of 'I.Interval's from 'notes' field.
intervals :: T3C -> Maybe [I.Interval]
intervals = I.intervals . notes

t3c_cmp :: T3C -> T3C -> Ordering
t3c_cmp = compare `on` date_time

-- * Parsing and formatting

-- | Parse T3C entry from CSV field list.  Note that an entry where
-- all but the date and note fields are blank is ignored as a comment.
t3c_parse :: [String] -> Maybe T3C
t3c_parse i =
    case i of
      [_,"     ","        ","   ","   ","   ","    ",_] -> Nothing
      [dt,tm,du,te,av,mx,en,nt] ->
          let dt' = parse_date_time (dt,tm)
              du' = parse_duration du
              te' = read te
              av' = read av
              mx' = read mx
              en' = read en
          in Just (T3C dt' du' te' av' mx' en' nt)
      _ -> error ("t3c_parse: " ++ show i)

-- | Format a 'T3C' value.
t3c_format :: T3C -> [String]
t3c_format (T3C dt du te av mx en n) =
    [format_date dt
    ,format_time dt
    ,format_duration du
    ,show te
    ,show (floor av::Int)
    ,show (floor mx::Int)
    ,show (floor en::Int)
    ,n]

-- | 'T3C' to CSV.
t3c_csv :: T3C -> String
t3c_csv = intercalate "," . t3c_format

-- | Print 'T3C' entry as CSV
t3c_print :: T3C -> IO ()
t3c_print = print . t3c_csv

t3c_load :: FilePath -> IO [T3C]
t3c_load fn = do
  (Right r) <- parseCSVFromFile fn
  return (mapMaybe t3c_parse r)

-- * Selection functions (t3c predicates)

t3c_date_cmp_p :: (Day -> Day -> Bool) -> UTCTime -> T3C -> Bool
t3c_date_cmp_p f d i =
    let i' = utctDay (date_time i)
    in i' `f` utctDay d

t3c_date_after_p :: UTCTime -> T3C -> Bool
t3c_date_after_p = t3c_date_cmp_p (>=)

t3c_date_before_p :: UTCTime -> T3C -> Bool
t3c_date_before_p = t3c_date_cmp_p (<=)

t3c_date_within_p :: (UTCTime,UTCTime) -> T3C -> Bool
t3c_date_within_p (l,r) i = t3c_date_after_p l i && t3c_date_before_p r i

t3c_week_starting :: UTCTime -> T3C -> Bool
t3c_week_starting t = t3c_date_within_p (t,add_days 6 t)

t3c_note_includes_p :: String -> T3C -> Bool
t3c_note_includes_p n i = n `isInfixOf` notes i

by_date :: (String,String) -> T3C -> Bool
by_date (l,r) = t3c_date_within_p (parse_date l,parse_date r)

week_starting :: String -> T3C -> Bool
week_starting = t3c_week_starting . parse_date

-- * IO interaction

-- | Sorts set of 'T3C' before processing.
with_hr :: FilePath -> ([T3C] -> a) -> IO a
with_hr fn f = fmap (f . sortBy t3c_cmp) (t3c_load fn)

with_t3c_io :: FilePath -> ([T3C] -> IO a) -> IO a
with_t3c_io fn = join . with_hr fn

-- * Summary

data Summary = Summary {s_start :: UTCTime
                       ,s_entries :: Int
                       ,s_dur :: Stat R
                       ,s_dur_t :: R
                       ,s_t3c_avg :: Stat R
                       ,s_t3c_max :: Stat R
                       ,s_te :: Stat R
                       ,s_en :: Stat R
                       ,s_en_t :: R}

mk_summary :: [T3C] -> Summary
mk_summary r =
    let st = minimum (map date_time r)
        ne = length r
        du = stat "dur" duration_h r
        du_t = sum (map duration_h r)
        ha = stat "hr-avg" hr_average r
        hm = stat "hr-max" hr_maximum r
        te = stat_nz 1 "te" training_effect r
        en = stat_nz 0 "en" energy r
        en_t = sum (map energy r)
    in Summary st ne du du_t ha hm te en en_t

summary_PP :: Summary -> String
summary_PP s =
  let l = [show ("start",s_start s)
          ,show ("entries",s_entries s)
          ,show (stat_map format_hours (s_dur s))
          ,show (s_t3c_avg s)
          ,show (s_t3c_max s)
          ,show (s_en s)
          ,show (s_te s)
          ,show ("total en:",s_en_t s)
          ,show ("total dur:",format_hours (s_dur_t s))]
  in unlines l

instance Show Summary where
    show = summary_PP

summary :: FilePath -> (T3C -> Bool) -> IO ()
summary fn p = with_t3c_io fn (putStrLn . summary_PP . mk_summary . filter p)

cons_maybe :: Maybe a -> [a] -> [a]
cons_maybe p q =
    case p of
      Just e -> e : q
      Nothing -> q

-- | Requires that the 'T3C' set be sorted by date.
weekly_summary_from :: UTCTime -> [T3C] -> [Summary]
weekly_summary_from t r =
    let f = t3c_week_starting t
        r' = dropWhile (not . f) r
        (p,q) = span f r'
        p' = if null p then Nothing else Just (mk_summary p)
    in case q of
         [] -> catMaybes [p']
         _ -> cons_maybe p' (weekly_summary_from (add_days 7 t) q)

weekly_summary_from' :: String -> [T3C] -> [Summary]
weekly_summary_from' t = weekly_summary_from (parse_date t)
