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

dispatch :: State -> String -> [(String,String)] -> CGI CGIResult
dispatch _ _ st =
    let f df nm = read (fromMaybe df (lookup nm st))
        chart = fromMaybe "gearing" (lookup "chart" st)
        o = case chart of
              "cadence" -> 
                  let c = f "60.0" "cadence"
                  in C.mk_gearing_chart (C.mk_cadence c)
              "gearing" -> 
                  let c_min = f "60.0" "cadence-minima"
                      c_max = f "110.0" "cadence-maxima"
                      v = f "36.0" "velocity"
                      g = (c_min, c_max, v)
                  in C.mk_gearing_chart (C.mk_gearing g)
              "gradient" -> 
                  let t = f "0.05" "tolerance"
                      m_r = f "62.0" "rider-weight"
                      m_b = f "8.0" "bicycle-weight"
                      m_k = f "2.0" "kit-weight"
                      m = m_r + m_b + m_k
                      w = f "250.0" "power"
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
