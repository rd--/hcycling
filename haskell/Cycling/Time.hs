module Cycling.Time where

import qualified Text.Printf as P
import qualified Data.List.Split as S

data HMSM i = HMSM i i i i

-- seconds -> seconds, milliseconds
s_sms :: (Integral i) => Double -> (i,i)
s_sms s =
    let s' = floor s
        ms = round ((s - fromIntegral s') * 1000)
    in (s',ms)

sms_s :: (Integral i) => (i,i) -> Double
sms_s (s,ms) = fromIntegral s + fromIntegral ms / 1000

-- allows either "H:M:S.MS" or "M:S.MS" or "S.MS"
parse_hms :: (Read i,Integral i) => String -> (i,i,i,i)
parse_hms x =
    let f (h,m,s) = let (s',ms) = s_sms s in (h,m,s',ms)
    in case S.splitOneOf ":" x of
        [h,m,s] -> f (read h,read m,read s)
        [m,s] -> f (0,read m,read s)
        [s] -> f (0,0,read s)
        _ -> error "parse_hms"

parse_hmsm :: (Read i,Num i) => String -> HMSM i
parse_hmsm =
    let f :: (Num i) => Int -> i
        f = fromIntegral
        g (i,j,k,l) = HMSM (f i) (f j) (f k) (f l)
    in g . parse_hms

instance (Num i,Read i) => Read (HMSM i) where
    readsPrec _ x = [(parse_hmsm x,"")]

show_hmsm :: Integral a => HMSM a -> String
show_hmsm (HMSM h m s ms) =
    let f :: Int -> String
        f = P.printf "%02d"
        g = f . fromIntegral
        s' = sms_s (s,ms)
    in concat [g h,":",g m,":",P.printf "%.3f" s']

instance (Integral i) => Show (HMSM i) where
    show = show_hmsm

hmsm_tuple :: (a -> b) -> HMSM a -> (b,b,b,b)
hmsm_tuple f (HMSM h m s ms) = (f h,f m,f s,f ms)
