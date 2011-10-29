import qualified Cycling.Chart as C
import Data.Maybe
import qualified Data.URLEncoded as U
import Network.CGI
import Network.URI

utf8_output :: String -> CGI CGIResult
utf8_output s = do
  setHeader "Content-type" "text/html; charset=utf-8"
  output s

type State = ()
type Query = [(String,String)]

revise_opt :: C.OPT -> Query -> C.OPT
revise_opt o qr =
   let get_f df nm = fromMaybe df (lookup nm qr)
   in map (\(x,y,m) -> (x,get_f y x,m)) o

dispatch :: State -> String -> [(String,String)] -> CGI CGIResult
dispatch _ _ qr =
    let c = case lookup "chart" qr of
              Just "cadence" ->
                  let o = revise_opt C.cadence_opt qr
                  in C.mk_cadence_chart o
              Just "cadence-tyre" ->
                  let o = revise_opt C.cadence_tyre_opt qr
                  in C.mk_cadence_tyre_chart o
              Just "gearing-cadence" ->
                  let o = revise_opt C.gearing_cadence_opt qr
                  in C.mk_gearing_cadence_chart o
              Just "gearing-measurements" ->
                  let o = revise_opt C.gearing_measurements_opt qr
                  in C.mk_gearing_measurements_chart o
              Just "gradient" ->
                  let o = revise_opt C.gradient_opt qr
                  in C.mk_gradient_chart o
              Just "velocita-ascensionale-media" ->
                  let o = revise_opt C.vam_opt qr
                  in C.mk_vam_chart o
              Just "average-velocity" ->
                  let o = revise_opt C.avg_vel_opt qr
                  in C.mk_avg_vel_chart o
              _ -> C.mk_index
    in utf8_output c

del_qm :: String -> String
del_qm s =
    case s of
      '?':s' -> s'
      _ -> s

cgi_main :: State -> CGI CGIResult
cgi_main l = do
  method <- requestMethod
  u <- queryURI
  q <- U.importString (del_qm (uriQuery u))
  dispatch l method (U.pairs q)

main :: IO ()
main = do
  runCGI (handleErrors (cgi_main ()))
