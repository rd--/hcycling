module Cycling (OPT
               ,cadence_opt,mk_cadence_chart
               ,gearing_opt,mk_gearing_chart
               ,gradient_opt,mk_gradient_chart
               ,mk_index) where

import qualified Data.List as L
import qualified Gearing as G
import qualified Power as P
import qualified Text.Printf as P
import qualified Text.HTML.Light as H
import qualified Text.XML.Light as X

std_html_attr :: [X.Attr]
std_html_attr = [H.xmlns "http://www.w3.org/1999/xhtml"
                ,H.xml_lang "en"
                ,H.lang "en" ]

css :: X.Content
css = H.link [H.rel "stylesheet"
             ,H.type' "text/css"
             ,H.href "css/cycling.css"]

mk_chart :: X.Content -> [String] -> [[String]] -> String
mk_chart fm t g =
    let mk_tr xs = H.tr [] (map (\x -> H.td [] [H.cdata x]) xs)
        th = H.tr [] (map (\x -> H.th [] [H.cdata x]) t)
        hd = H.head [] [H.title [] [H.cdata "cycling"], css]
        bd = H.body [] [fm, H.table [] (th : map mk_tr g)]
        h = H.html std_html_attr [hd, bd]
    in H.renderXHTML H.xhtml_1_0_strict h

type R = Double
type VAR = (String,R)
type OPT = [VAR]

opt_form :: [(String,String)] -> OPT -> X.Content
opt_form z o =
    let mk_h (k,v) = H.input [H.type' "hidden"
                             ,H.name k
                             ,H.value v]
        mk_s (k,v) =
            H.dl
             []
             [H.dt
              [H.class' "key"]
              [H.cdata k]
             ,H.dd
              []
              [H.input
               [H.type' "text"
               ,H.name k
               ,H.value v]]]
        mk_r (k,v) = mk_s (k,show v)
        sb = H.input [H.class' "submit"
                     ,H.type' "submit"
                     ,H.value "calculate"]
    in H.form
         [H.action "./", H.method "get"]
         (map mk_h z ++ map mk_r o ++ [sb])

gradient_opt :: OPT
gradient_opt =
    [("tolerance", 0.05)
    ,("rider-weight", 62)
    ,("bicycle-weight", 8)
    ,("kit-weight", 2)
    ,("power", 250)]

mk_gradient :: OPT -> [(R, R, R)]
mk_gradient o =
  let [t, m_r, m_b, m_k, w] = map snd o
      m = m_r + m_b + m_k
      gs = [0, 0.5 .. 20]
      f g = let (v, w') = P.velocity_std t m g w
            in (g, v, w')
  in map f gs

mk_gradient_chart :: OPT -> String
mk_gradient_chart o =
    let f = P.printf "%.1f"
        gs = mk_gradient o
        gs' = map (\(g,c,v) -> [f g, f c, f v]) gs
        fm = opt_form [("chart","gradient")] o
    in mk_chart fm ["gradient", "velocity", "power"] gs'

gearing_opt :: OPT
gearing_opt =
    [("cadence-minima", 60)
    ,("cadence-maxima", 110)
    ,("velocity", 36)]

mk_gearing :: OPT -> [(G.Gear, Double, Double)]
mk_gearing o =
  let [c_min, c_max, v] = map snd o
      t_23_622 = G.Tyre 23 622
      cw = [39,53]
      cs_12_25 = [12,13,14,15,16,17,19,21,23,25]
      gs = [G.Gear c s | c <- cw, s <- cs_12_25]
      valid_c c = c >= c_min && c <= c_max
      rs = [(g, G.cadence t_23_622 g v, v) | g <- gs]
  in filter (\(_,c,_) -> valid_c c) rs

mk_gearing_chart :: OPT -> String
mk_gearing_chart o =
    let f = P.printf "%.1f"
        gs = mk_gearing o
        gs' = map (\(g,c,v) -> [show g, f c, f v]) gs
        fm = opt_form [("chart","gearing")] o
    in mk_chart fm ["gear", "cadence", "velocity"] gs'

cadence_opt :: OPT
cadence_opt =
    [("cadence", 60)]

mk_cadence :: OPT -> [(G.Gear, Double, Double)]
mk_cadence o =
  let [c] = map snd o
      t_23_622 = G.Tyre 23 622
      cw = [39,53]
      cs_12_25 = [12,13,14,15,16,17,19,21,23,25]
      gs = [G.Gear r s | r <- cw, s <- cs_12_25]
      cmp (_,_,x) (_,_,y) = compare x y
  in L.sortBy cmp [(g, c, G.velocity t_23_622 g c) | g <- gs]

mk_cadence_chart :: OPT -> String
mk_cadence_chart o =
    let f = P.printf "%.1f"
        gs = mk_cadence o
        gs' = map (\(g,c,v) -> [show g, f c, f v]) gs
        fm = opt_form [("chart","cadence")] o
    in mk_chart fm ["gear", "cadence", "velocity"] gs'

mk_index :: String
mk_index =
    let cs = L.sort ["cadence", "gearing", "gradient"]
        mk_ln c = H.li [] [H.a [H.href ("?chart="++c)] [H.cdata c]]
        hd = H.head [] [H.title [] [H.cdata "cycling"], css]
        bd = H.body [] [H.ul [] (map mk_ln cs)]
        h = H.html std_html_attr [hd, bd]
    in H.renderXHTML H.xhtml_1_0_strict h
