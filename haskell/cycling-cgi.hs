import qualified Cycling as C
import Data.Maybe
import qualified Data.URLEncoded as U
import Network.CGI
import Network.URI

utf8_output :: String -> CGI CGIResult
utf8_output s = do
  setHeader "Content-type" "text/html; charset=utf-8"
  output s

type State = ()

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
    case reads s of
      [(i,"")] -> Just i
      _ -> Nothing

dispatch :: State -> String -> [(String,String)] -> CGI CGIResult
dispatch _ _ st =
    let get_f df nm = fromMaybe df (lookup nm st >>= readMaybe)
        chart = fromMaybe "gearing" (lookup "chart" st)
        o = case chart of
              "cadence" -> 
                  let c = get_f 60 "cadence"
                  in C.mk_gearing_chart (C.mk_cadence c)
              "gearing" -> 
                  let c_min = get_f 60 "cadence-minima"
                      c_max = get_f 110 "cadence-maxima"
                      v = get_f 36 "velocity"
                      g = (c_min, c_max, v)
                  in C.mk_gearing_chart (C.mk_gearing g)
              "gradient" -> 
                  let t = get_f 0.05 "tolerance"
                      m_r = get_f 62 "rider-weight"
                      m_b = get_f 8 "bicycle-weight"
                      m_k = get_f 2 "kit-weight"
                      m = m_r + m_b + m_k
                      w = get_f 250 "power"
                  in C.mk_gradient_chart (C.mk_gradient (t, m, w))
              _ -> undefined
    in utf8_output o

del_qm :: String -> String
del_qm ('?':xs) = xs
del_qm xs = xs

cgi_main :: State -> CGI CGIResult
cgi_main l = do
  method <- requestMethod
  u <- queryURI
  q <- U.importString (del_qm (uriQuery u))
  dispatch l method (U.pairs q)

main :: IO ()
main = do
  runCGI (handleErrors (cgi_main ()))
