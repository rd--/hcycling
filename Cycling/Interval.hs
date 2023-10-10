-- | Parser for 'Interval' lists.
module Cycling.Interval where

import Control.Monad {- base -}
import Data.Maybe {- base -}
import Prelude hiding ((<>)) {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.String as String {- parsec -}

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

-- | A 'Char' parser with no user state.
type P a = String.GenParser Char () a

{- | Parse an 'Integer'

>>> P.parse integer "" "23"
Right 23
-}
integer :: P Integer
integer = fmap read (P.many (P.oneOf ['0'..'9']))

{- | Parse the optional /cadence/ entry.

>>> P.parse cadence "" "^116"
Right (Just 116)

>>> P.parse cadence "" ""
Right Nothing
-}
cadence :: P (Maybe Integer)
cadence = P.option Nothing (P.char '^' >> fmap Just integer)

-- | Left nested duple to triple.
to_triple :: ((a,b),c) -> (a,b,c)
to_triple ((i,j),k) = (i,j,k)

-- | Left nested duple to 'Interval'.
to_interval :: ((Integer,Integer),Maybe Integer) -> Interval
to_interval ((i,j),k) = Interval i j k

{- | Parse an 'Interval'

>>> P.parse interval "" "23@156"
Right (23@156)

>>> P.parse interval "" "23@156^116"
Right (23@156)^116
-}
interval :: P Interval
interval = do
  lhs <- integer
  _ <- P.char '@'
  rhs <- integer
  c <- cadence
  return (to_interval ((lhs, rhs), c))

{- | Parse a 'Char' separated list of /e/ elements.

>>> P.parse (list ',' integer) "" "[23,8,3]"
Right [23,8,3]

>>> let r = [Interval 23 156 (Just 116)]
>>> P.parse (list ';' interval) "" "[23@156^116]"
Right [23@156^116]
-}
list :: Char -> P t -> P [t]
list c e = do
  _ <- P.char '['
  answer <- P.sepBy1 e (P.char c)
  _ <- P.char ']'
  return answer

{- | Remove commentary prior to initial @[@.

>>> P.parse rem_pre "" "pre [] post"
Right ()
-}
rem_pre :: P ()
rem_pre = P.many (P.noneOf "[") >> return ()

{- | Parse list of 'Interval's, discarding any pre- and post-ambles.

>>> P.parse interval_list "" "[23@156;8@148]"
Right [23@156,8@148]
-}
interval_list :: P [Interval]
interval_list = do
  _ <- rem_pre
  answer <- list ';' interval
  _ <- P.getInput
  return answer

{- | Run 'interval_list' parser.

>>> intervals "pre [23@156;8@148] post"
Just [23@156,8@148]

>>> intervals "no interval"
Nothing

>>> intervals ""
Nothing
-}
intervals :: String -> Maybe [Interval]
intervals = either (const Nothing) Just . P.parse interval_list ""

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
