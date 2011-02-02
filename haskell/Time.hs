module Time where

import qualified Data.List.Split as S

-- allows either "H:M:S.MS" or "M:S.MS"
parse_hms :: (Read i,Num i) => String -> (i,i,i,i)
parse_hms x =
    case S.splitOneOf ":." x of
      [h,m,s,ms] -> (read h,read m,read s,read ms)
      [m,s,ms] -> (0,read m,read s,read ms)
      [m,s] -> (0,read m,read s,0)
      [m] -> (0,read m,0,0)
      _ -> error "parse_hms"
