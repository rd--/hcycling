-- | General purpose analysis functions.
module Cycling.Analysis where

import Data.Function
import Data.List
import Data.Maybe

-- | A synonym for 'Double'.
type R = Double

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

-- * Chart utilities

normalise :: (Fractional a) => (a,a) -> a -> a
normalise (l,r) x =
    let i = r - l
    in (x - l) / i

normalise_tr :: (Fractional a) => (a,a) -> (a,a,a) -> (a,a,a)
normalise_tr r (x,z,y) = let f = normalise r in (f x,f y,f z)

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
