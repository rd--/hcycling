import qualified Cycling.Chart as C {- hcycling -}
import Data.Maybe {- base -}
import qualified WWW.Minus.CGI as W {- www-minus -}

type State = ()

revise_opt :: C.OPT -> W.Query -> C.OPT
revise_opt o qr =
   let get_f df nm = fromMaybe df (lookup nm qr)
   in map (\(k,v,u,m) -> (k,get_f v k,u,m)) o

dispatch :: State -> W.Parameters -> IO ()
dispatch _ (_,qr) =
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
              Just "elapsed-time-comparison" ->
                  let o = revise_opt C.et_cmp_opt qr
                  in C.mk_et_cmp_chart o
              _ -> C.mk_index
    in W.utf8_html_output c

main :: IO ()
main = W.cgi_main (dispatch ())
