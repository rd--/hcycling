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

revise_opt :: C.OPT -> [(String,String)] -> C.OPT
revise_opt o st =
   let get_f df nm = fromMaybe df (lookup nm st >>= readMaybe)
   in map (\(x,y) -> (x, get_f y x)) o

dispatch :: State -> String -> [(String,String)] -> CGI CGIResult
dispatch _ _ st =
    let c = case lookup "chart" st of
              Just "cadence" ->
                  let o = revise_opt C.cadence_opt st
                  in C.mk_cadence_chart o
              Just "gearing" ->
                  let o = revise_opt C.gearing_opt st
                  in C.mk_gearing_chart o
              Just "gradient" ->
                  let o = revise_opt C.gradient_opt st
                  in C.mk_gradient_chart o
              _ -> C.mk_index
    in utf8_output c

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
