-- | Parser for 'Interval' lists.
module Cycling.Interval where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import Prelude hiding ((<>)) {- base -}
import Text.Parsers.Frisby {- frisby -}

-- | An 'Interval' is a /duration/, an /average heart rate/ and
-- perhaps a /cadence/.
data Interval = Interval {interval_duration :: Integer
                         ,interval_hr :: Integer
                         ,interval_cadence :: Maybe Integer}
              deriving (Eq)

-- | Pretty print 'Interval'.  This is the 'Show' instance.
--
-- > intervalPP (Interval 30 160 (Just 90)) == "30@160^90"
intervalPP :: Interval -> String
intervalPP (Interval d hr c) =
    let c' = maybe "" (('^' :) . show) c
    in show d ++ "@" ++ show hr ++ c'

instance Show Interval where
    show = intervalPP

-- | Parse an 'Integer'
--
-- > runPeg (newRule integer) "23" == 23
integer :: P s Integer
integer = many (oneOf ['0'..'9']) ## read

-- | Parse the optional /cadence/ entry.
--
-- > runPeg (newRule cadence) "^116" == Just 116
-- > runPeg (newRule cadence) "" == Nothing
cadence :: P s (Maybe Integer)
cadence = option Nothing (char '^' ->> fmap Just integer)

-- | Left nested duple to triple.
to_triple :: ((a,b),c) -> (a,b,c)
to_triple ((i,j),k) = (i,j,k)

-- | Left nested duple to 'Interval'.
to_interval :: ((Integer,Integer),Maybe Integer) -> Interval
to_interval ((i,j),k) = Interval i j k

-- | Parse an 'Interval'
--
-- > runPeg (newRule interval) "23@156" == Interval 23 156 Nothing
-- > runPeg (newRule interval) "23@156^116" == Interval 23 156 (Just 116)
interval :: P s Interval
interval = fmap to_interval ((integer <<- char '@' <> integer) <> cadence)

-- | Parse a 'Char' separated list of /e/ elements.
--
-- > runPeg (newRule (list ',' integer)) "[23,8,3]" == [23,8,3]
--
-- > let r = [Interval 23 156 (Just 116)]
-- > in runPeg (newRule (list ';' interval)) "[23@156^116]" == r
list :: Char -> P s b -> P s [b]
list c e =
    let i_element = e <<- char c
        t_element = e <<- char ']'
        p_append (i,j) = i ++ [j]
        list_c = char '[' ->> many i_element <> t_element
    in list_c ## p_append

-- | Remove commentary prior to initial @[@.
--
-- > runPeg (newRule (rem_pre ->> rest)) "pre [] post" == "[] post"
rem_pre :: P s ()
rem_pre = discard (many (noneOf "["))

-- | Parse list of 'Interval's, discarding any pre- and post-ambles.
--
-- > let r = [Interval 23 156 Nothing
-- >         ,Interval 8 148 Nothing]
-- > in runPeg (newRule interval_list) "[23@156;8@148]" == r
interval_list :: P s [Interval]
interval_list = rem_pre ->> list ';' interval <<- rest

-- | Run 'interval_list' parser.
--
-- > intervals "pre [23@156;8@148] post" == Just [Interval 23 156 Nothing
-- >                                             ,Interval 8 148 Nothing]
--
-- > intervals "no interval" == Nothing
-- > intervals "" == Nothing
intervals :: String -> Maybe [Interval]
intervals =
    let i = fmap Just (interval_list <<- eof) // unit Nothing
    in runPeg (newRule i)

-- | Given duration /field/ function transform sequence into adjacent
-- /(start,end)/ duples.
--
-- > adjacencies fst [(11,157),(12,158)] == [(0,11),(11,23)]
adjacencies :: Num b => (a -> b) -> [a] -> [(b,b)]
adjacencies f =
    let g n i = case i of
                  [] -> []
                  x:i' -> let d = f x
                              n' = d + n
                          in (n,n') : g n' i'
    in g 0

-- | First element of triple.
fst3 :: (t,t1,t2) -> t
fst3 (i,_,_) = i

-- | Variant on 'adjacencies' that stores initial entry.
intervals_adjacent :: [Interval] -> [((Integer,Integer),Interval)]
intervals_adjacent i = zip (adjacencies interval_duration i) i

-- | Total duration of a set of 'Interval's.
--
-- > let i = [Interval 23 156 Nothing,Interval 8 148 Nothing]
-- > in intervals_duration i == 31
intervals_duration :: [Interval] -> Integer
intervals_duration = sum . map interval_duration

-- | Variant of 'intervals_duration'.
--
-- > intervals_duration_m Nothing == 0
intervals_duration_m :: Maybe [Interval] -> Integer
intervals_duration_m = fromMaybe 0 . liftM intervals_duration
