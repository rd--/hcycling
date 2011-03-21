module Time where

import qualified Data.List.Split as S

s_ms :: (Integral i) => Double -> (i,i)
s_ms s =
    let s' = floor s
        ms = round (s - fromIntegral s')
    in (s',ms)

-- allows either "H:M:S.MS" or "M:S.MS" or "M"
parse_hms :: (Read i,Integral i) => String -> (i,i,i,i)
parse_hms x =
    let f (h,m,s) = let (s',ms) = s_ms s in (h,m,s',ms)
    in case S.splitOneOf ":" x of
        [h,m,s] -> f (read h,read m,read s)
        [m,s] -> f (0,read m,read s)
        [m] -> f (0,read m,0)
        _ -> error "parse_hms"

parse_hms' :: (Read i,Num i) => String -> (i,i,i,i)
parse_hms' =
    let f :: (Num i) => Int -> i
        f = fromIntegral
        g (i,j,k,l) = (f i,f j,f k,f l)
    in g . parse_hms