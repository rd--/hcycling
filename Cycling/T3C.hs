-- | Suunto T3C HR monitor related functions
module Cycling.T3C where

import Control.Monad
import qualified Data.Colour.Names as N {- colour -}
import qualified Cycling.Interval as I
import Cycling.Plot
import Cycling.Time
import Data.Function
import Data.List
import Data.Maybe
import Data.Time {- time -}
import qualified Graphics.Rendering.Chart.Simple as C {- chart -}
import Text.CSV {- csv -}
import Text.Printf


-- * T3C data type

data T3C = T3C {date_time :: UTCTime
               ,duration :: DiffTime
               ,training_effect :: R
               ,hr_average :: R
               ,hr_maximum :: R
               ,energy :: R
               ,notes :: String }
           deriving (Eq,Show)

-- | Real valued duration (hours).
duration_h :: T3C -> R
duration_h = diff_time_hours . duration

-- | Real valued time-stamp (days).
time_stamp :: T3C -> R
time_stamp hr =
    let t = date_time hr
        d = utctDay t
        d' = fromIntegral (toModifiedJulianDay d)
        s = utctDayTime t
        s_max = 86401
    in d' + (fromRational (toRational s) / s_max)

-- | Parse list of 'I.Interval's from 'notes' field.
intervals :: T3C -> Maybe [I.Interval]
intervals = I.intervals . notes

-- * Parsing and formatting

-- | Parse T3C entry
t3c_parse :: [String] -> Maybe T3C
t3c_parse i =
    case i of
      [dt,tm,du,te,av,mx,en,nt] ->
          let dt' = parse_date_time (dt,tm)
              du' = parse_duration du
              te' = read te
              av' = read av
              mx' = read mx
              en' = read en
          in Just (T3C dt' du' te' av' mx' en' nt)
      [_,"                               ",_] -> Nothing
      _ -> error ("t3c_parse: " ++ show i)

-- | Format E value.
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

-- | E to CSV.
t3c_csv :: T3C -> String
t3c_csv = intercalate "," . t3c_format

-- | Print T3C entry as CSV
t3c_print :: T3C -> IO ()
t3c_print = print . t3c_csv

t3c_load :: IO [T3C]
t3c_load = do
  (Right r) <- parseCSVFromFile "hr-data.csv"
  return (mapMaybe t3c_parse r)

-- * Selection functions (t3c predicates)

t3c_date_cmp_p :: (Day -> Day -> Bool) -> UTCTime -> (T3C -> Bool)
t3c_date_cmp_p f d i =
    let i' = utctDay (date_time i)
    in i' `f` utctDay d

t3c_date_after_p :: UTCTime -> (T3C -> Bool)
t3c_date_after_p d = t3c_date_cmp_p (>=) d

t3c_date_befort3c_p :: UTCTime -> (T3C -> Bool)
t3c_date_befort3c_p d = t3c_date_cmp_p (<=) d

t3c_date_within_p :: (UTCTime,UTCTime) -> (T3C -> Bool)
t3c_date_within_p (l,r) i = t3c_date_after_p l i && t3c_date_befort3c_p r i

t3c_week_starting :: UTCTime -> T3C -> Bool
t3c_week_starting t = t3c_date_within_p (t,add_days 6 t)

t3c_note_includes_p :: String -> (T3C -> Bool)
t3c_note_includes_p n i = n `isInfixOf` notes i

by_date :: (String,String) -> (T3C -> Bool)
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

with_hr :: ([T3C] -> a) -> IO a
with_hr f = t3c_load >>= return.f

with_t3c_io :: ([T3C] -> IO a) -> IO a
with_t3c_io = join . with_hr

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

summary :: (T3C -> Bool) -> IO ()
summary p = with_t3c_io (putStrLn . summary_PP . mk_summary . filter p)

weekly_summary_from :: UTCTime -> [T3C] -> [Summary]
weekly_summary_from t r =
    case filter (t3c_week_starting t) r of
      [] -> []
      r' -> mk_summary r' : weekly_summary_from (add_days 7 t) r

weekly_summary_from' :: String -> [T3C] -> [Summary]
weekly_summary_from' t = weekly_summary_from (parse_date t)

-- * Chart

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

t3c_chart :: ((Int,T3C) -> R) -> [T3C] -> IO ()
t3c_chart fx r = do
  let nm rng = map (normalise rng)
      a = nm (0,hr_limit) (map hr_average r)
      m = nm (0,hr_limit) (map hr_maximum r)
      d = nm (0,10) (map (diff_time_hours . duration) r)
      e = nm (0,5000) (map energy r)
      t = nm (1,5) (map training_effect r)
  print (mk_summary r)
  C.plotWindow (map fx (zip [0..] r))
       a (hr_range_txt "avg" hr_limit) C.Plus -- C.Solid
       m (hr_range_txt "max" hr_limit) C.Plus -- C.Solid
       d "dur (0-10)" C.HollowCircle -- C.Solid
       e "energy (0-4000)" C.Triangle -- C.Solid
       t "te (1-5)" C.DownTriangle -- C.Solid

sort_on :: (Ord b) => (a -> b) -> [a] -> [a]
sort_on f = sortBy (compare `on` f)

t3c_plot :: (T3C -> Bool) -> IO ()
t3c_plot p = do
  r <- t3c_load
  let r' = filter p (sort_on time_stamp r)
  t3c_chart (time_stamp . snd) r'

t3c_plot' :: (Ord a) => (T3C -> Bool) -> (T3C -> a) -> IO ()
t3c_plot' p cmp = do
  r <- t3c_load
  let r' = filter p (sort_on cmp r)
  t3c_chart (fromIntegral . fst) r'

t3c_plot_by_date :: (String,String) -> IO ()
t3c_plot_by_date rg = t3c_plot (by_date rg)

-- * Chart (summary)

-- Just "hr-chart-summary.pdf"
t3c_chart_summary :: [Summary] -> IO ()
t3c_chart_summary s = do
  let un_st = snd -- (_,(i,k,j)) = (i,j,k)
      nm r f = map (normalise r . f)
      nm_tr r f = map (normalise_tr r . un_st . f)
      ne = mk_plot_pt "n-entries" N.purple (-0.1) (nm (0,16) (fromIntegral . s_entries) s)
      du = mk_plot_tr "dur" N.green 0.0 (nm_tr (0,10) s_dur s)
      du_t = mk_plot_pt "dur-t" N.green 0.0 (nm (0,30) s_dur_t s)
      ha = mk_plot_tr "hr-avg" N.blue 0.1 (nm_tr (80,hr_limit) s_t3c_avg s)
      hm = mk_plot_tr "hr-max" N.red 0.2 (nm_tr (80,hr_limit) s_t3c_max s)
      en = mk_plot_tr "en" N.yellow 0.3 (nm_tr (100,5000) s_en s)
      en_t = mk_plot_pt "en-t" N.yellow 0.3 (nm (100,12000) s_en_t s)
      te = mk_plot_tr "te" N.aqua 0.4 (nm_tr (1,5) s_te s)
  mk_chart (1024,576) Nothing [ne,du,du_t,ha,hm,en,en_t,te]

-- * Predicate logic
