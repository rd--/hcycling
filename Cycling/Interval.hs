-- | Parser for 'Interval' lists.
module Cycling.Interval where

import Text.Parsers.Frisby {- frisby -}

-- | An 'Interval' is a /duration/ and /average heart rate/ tuple.
type Interval = (Integer,Integer)

-- | Parse an 'Integer'
--
-- > runPeg (newRule integer) "23" == 23
integer :: P s Integer
integer = many (oneOf ['0'..'9']) ## read

-- | Parse an 'Interval'
--
-- > runPeg (newRule interval) "23@156" == (23,156)
interval :: P s Interval
interval = integer <<- char '@' <> integer

-- | Parse a 'Char' separated list of /e/ elements.
--
-- > runPeg (newRule (list ',' integer)) "[23,8,3]" == [23,8,3]
-- > runPeg (newRule (list ';' interval)) "[23@15;8@14]" == [(23,15),(8,14)]
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
-- > runPeg (newRule interval_list) "[23@156;8@148]" == [(23,156),(8,148)]
interval_list :: P s [(Integer, Integer)]
interval_list = rem_pre ->> list ';' interval <<- rest

-- | Run 'interval_list' parser.
--
-- > intervals "pre [23@156;8@148] post" == Just [(23,156),(8,148)]
-- > intervals "no interval" == Nothing
-- > intervals "" == Nothing
intervals :: String -> Maybe [Interval]
intervals =
    let i = fmap Just (interval_list <<- eof) // unit Nothing
    in runPeg (newRule i)
