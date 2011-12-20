-- | General purpose analysis functions.
module Cycling.Analysis where

import Data.Function
import Data.List
import Data.Maybe

-- | A synonym for 'Double'.
type R = Double

-- * Statistics

-- | Simple (minima,maxima,average) statistics.
type Stat n = (String,(n,n,n))

-- | Generate 'Stat' for given data.
--
-- > stat_m "x" Just [0..10] == ("x",(0.0,10.0,5.0))
stat_m :: String -> (a -> Maybe R) -> [a] -> Stat R
stat_m tx fn xs =
    let xs' = mapMaybe fn xs
        mn = minimum xs'
        mx = maximum xs'
        av = sum xs' / fromIntegral (length xs')
    in (tx,(mn,mx,av))

-- | Non-maybe variant of 'stat_m'.
stat :: String -> (b -> R) -> [b] -> Stat R
stat tx fn = stat_m tx (Just . fn)

-- | Non-zero filter variant of 'stat_m'.
stat_nz :: R -> String -> (b -> R) -> [b] -> Stat R
stat_nz z tx fn =
    let fn' i = let i' = fn i
                in if i' == z then Nothing else Just i'
    in stat_m tx fn'

-- | Apply function at each node of 'Stat'.
--
-- > stat_map negate ("x",(0,10,5)) == ("x",(-0,-10,-5))
stat_map :: (a -> b) -> Stat a -> Stat b
stat_map f (x,(s1,s2,s3)) = (x,(f s1,f s2,f s3))

-- * Chart utilities

-- | Compute 'minimum' and 'maximum' of input.
--
-- > minimum_maximum [1..10] == (1,10)
minimum_maximum :: Ord t => [t] -> (t,t)
minimum_maximum l = (minimum l,maximum l)

-- | Normalise data to @(0,1)@ over @(left,right)@ pair.
--
-- > normalise (0,10) 5 == 0.5
normalise :: (Fractional a) => (a,a) -> a -> a
normalise (l,r) x =
    let i = r - l
    in (x - l) / i

-- | Variant of 'normalise' over 'Maybe'.
--
-- > map (normalise_m (0,10)) [Just 5,Nothing] == [Just 0.5,Nothing]
normalise_m :: (Fractional a) => (a,a) -> Maybe a -> Maybe a
normalise_m r = fmap (normalise r)

-- | Normalise to range of input.
--
-- > normalise_r [0,10,5] == [0,1,0.5]
normalise_r :: (Ord a,Fractional a) => [a] -> [a]
normalise_r l = map (normalise (minimum_maximum l)) l

-- | Variant of 'normalise_r' over 'Maybe'.
--
-- > normalise_r_m [Just 0,Nothing,Just 10] == [Just 0,Nothing,Just 1]
normalise_r_m :: (Ord a, Fractional a) => [Maybe a] -> [Maybe a]
normalise_r_m l =
    let mm = minimum_maximum (catMaybes l)
    in map (normalise_m mm) l

-- | Triple variant of 'normalise'.
--
-- > normalise_tr (0,10) (0,5,10) == (0,0.5,1)
normalise_tr :: (Fractional a) => (a,a) -> (a,a,a) -> (a,a,a)
normalise_tr r (x,y,z) = let f = normalise r in (f x,f y,f z)

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
