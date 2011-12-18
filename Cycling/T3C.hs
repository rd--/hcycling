-- | Suunto T3C HR monitor related functions
module Cycling.T3C where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Time {- time -}
import Text.CSV {- csv -}
import Text.Printf

import qualified Cycling.Interval as I
import Cycling.Time

-- * T3C data type

-- | A synonym for 'Double'.
type R = Double

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

-- * Statistics

type Stat n = (String,(n,n,n))

stat_m :: String -> (a -> Maybe R) -> [a] -> Stat R
stat_m tx fn xs =
    let xs' = mapMaybe fn xs
        mn = minimum xs'
        mx = maximum xs'
        av = sum xs' / fromIntegral (length xs')
    in (tx,(mn,mx,av))

-- | Non-maybe variant
stat :: String -> (b -> R) -> [b] -> Stat R
stat tx fn = stat_m tx (Just . fn)

-- | non-zero filter variant
stat_nz :: R -> String -> (b -> R) -> [b] -> Stat R
stat_nz z tx fn =
    let fn' i = let i' = fn i
                in if i' == z then Nothing else Just i'
    in stat_m tx fn'

stat_map :: (a -> b) -> Stat a -> Stat b
stat_map f (x,(s1,s2,s3)) = (x,(f s1,f s2,f s3))

-- * IO interaction

with_hr :: FilePath -> ([T3C] -> a) -> IO a
with_hr fn f = fmap f (t3c_load fn)

with_t3c_io :: FilePath -> ([T3C] -> IO a) -> IO a
with_t3c_io fn = join . with_hr fn

-- * Summary

data Summary = Summary {s_entries :: Int
                       ,s_dur :: Stat R
                       ,s_dur_t :: R
                       ,s_t3c_avg :: Stat R
                       ,s_t3c_max :: Stat R
                       ,s_te :: Stat R
                       ,s_en :: Stat R
                       ,s_en_t :: R}

mk_summary :: [T3C] -> Summary
mk_summary r =
    let ne = length r
        du = stat "dur" duration_h r
        du_t = sum (map duration_h r)
        ha = stat "hr-avg" hr_average r
        hm = stat "hr-max" hr_maximum r
        te = stat_nz 1 "te" training_effect r
        en = stat_nz 0 "en" energy r
        en_t = sum (map energy r)
    in Summary ne du du_t ha hm te en en_t

summary_PP :: Summary -> String
summary_PP s =
  let l = [show ("entries",s_entries s)
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

weekly_summary_from :: UTCTime -> [T3C] -> [Summary]
weekly_summary_from t r =
    case filter (t3c_week_starting t) r of
      [] -> []
      r' -> mk_summary r' : weekly_summary_from (add_days 7 t) r

weekly_summary_from' :: String -> [T3C] -> [Summary]
weekly_summary_from' t = weekly_summary_from (parse_date t)

-- * Chart utilities

normalise :: (Fractional a) => (a,a) -> a -> a
normalise (l,r) x =
    let i = r - l
    in (x - l) / i

normalise_tr :: (Fractional a) => (a,a) -> (a,a,a) -> (a,a,a)
normalise_tr r (x,z,y) = let f = normalise r in (f x,f y,f z)

hr_limit :: R
hr_limit = 186

hr_range_txt :: String -> R -> String
hr_range_txt x n = printf "hr-%s (0-%d)" x (floor n :: Int)

-- | Variant that 'sort's on 'Ord' value extracted by /f/.
--
-- > sort_on snd [('a',1),('b',0)] == [('b',0),('a',1)]
sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
sort_on = sortBy . on compare

-- | Variant of 'zip' that discards elements from the /lhs/ list that
-- do not have a counterpart in the /rhs/ list.
--
-- > zipMaybe [1..] [Just 'a',Nothing,Just 'c'] == [(1,'a'),(3,'c')]
zipMaybe :: [a] -> [Maybe b] -> [(a,b)]
zipMaybe i j =
    case (i,j) of
      ([],_) -> []
      (_,[]) -> []
      (_:i',Nothing:j') -> zipMaybe i' j'
      (p:p',Just q:q') -> (p,q) : zipMaybe p' q'

-- * Analysis

-- | Predicate /and/.
--
-- > filter (odd `p_and` (> 6)) [1..10] == [7,9]
p_and :: (t -> Bool) -> (t -> Bool) -> t -> Bool
p_and f g x = f x && g x

-- | Given field function find set of maxima.
--
-- > maximaBy snd (zip ['a'..] [3,6,5,6]) == [('d',6),('b',6)]
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f = head .
             groupBy ((==) `on` f) .
             reverse .
             sortBy (compare `on` f)


-- | Variant of 'maximumBy' that 'compare's 'on' /field/.
--
-- > maximaByOn snd (zip ['a'..] [3,6,5,6]) == ('d',6)
maximaByOn :: Ord b => (a -> b) -> [a] -> a
maximaByOn f = maximumBy (compare `on` f)

-- | Extract data given comparator /c/, sort field /s/, comparison
-- field /f/ and comparison value /n/.
--
-- > extractor (>=) fst snd 4 (zip "abdc" [5,3,3,4]) == [('a',5),('c',4)]
extractor :: Ord b => (c->c->Bool) -> (a->b) -> (a->c) -> c -> [a] -> [a]
extractor c s f n = sort_on s . filter (\h -> f h `c` n)
