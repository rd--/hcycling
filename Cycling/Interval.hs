-- | Parser for 'Interval' lists.
module Cycling.Interval where

import Control.Monad
import Text.Parsers.Frisby {- frisby -}

-- | An 'Interval' is a /duration/, an /average heart rate/ and perhaps a /cadence/.
type Interval = (Integer,Integer,Maybe Integer)

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

-- | Left nested duple tto triple.
to_triple :: ((a,b),c) -> (a,b,c)
to_triple ((i,j),k) = (i,j,k)

-- | Parse an 'Interval'
--
-- > runPeg (newRule interval) "23@156" == (23,156,Nothing)
-- > runPeg (newRule interval) "23@156^116" == (23,156,Just 116)
interval :: P s Interval
interval = fmap to_triple ((integer <<- char '@' <> integer) <> cadence)

-- | Parse a 'Char' separated list of /e/ elements.
--
-- > runPeg (newRule (list ',' integer)) "[23,8,3]" == [23,8,3]
-- > runPeg (newRule (list ';' interval)) "[23@156^116]" == [(23,156,Just 116)]
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
-- > runPeg (newRule interval_list) "[23@156;8@148]" == [(23,156,Nothing),(8,148,Nothing)]
interval_list :: P s [Interval]
interval_list = rem_pre ->> list ';' interval <<- rest

-- | Run 'interval_list' parser.
--
-- > intervals "pre [23@156;8@148] post" == Just [(23,156,Nothing),(8,148,Nothing)]
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
intervals_adjacent i = zip (adjacencies fst3 i) i

-- | Total duration of a set of 'Interval's.
--
-- > intervals_duration [(23,156,Nothing),(8,148,Nothing)] == 31
intervals_duration :: [Interval] -> Integer
intervals_duration = sum . map fst3

-- | Variant of 'intervals_duration'.
--
-- > intervals_duration_m Nothing == 0
intervals_duration_m :: Maybe [Interval] -> Integer
intervals_duration_m = maybe 0 id . liftM intervals_duration
