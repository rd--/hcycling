module Cycling ( mk_cadence
               , mk_gearing
               , mk_gearing_chart
               , mk_gradient
               , mk_gradient_chart ) where

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

mk_chart :: [String] -> [[String]] -> String
mk_chart t g =
    let mk_tr xs = H.tr [] (map (\x -> H.td [] [H.cdata x]) xs)
        th = H.tr [] (map (\x -> H.th [] [H.cdata x]) t)
        hd = H.head [] [H.title [] [H.cdata "cycling"], css]
        bd = H.body [] [H.table [] (th : map mk_tr g)]
        h = H.html std_html_attr [hd, bd]
    in H.renderXHTML H.xhtml_1_0_strict h

type R = Double

mk_gradient :: (R, (R, R, R), R) -> [(R, R, R)]
mk_gradient (t, (m_r, m_b, m_k), w) =
  let m = m_r + m_b + m_k
      gs = [0, 0.5 .. 20]
      f g = let (v, w') = P.velocity_std t m g w 
            in (g, v, w')
  in map f gs

mk_gradient_chart :: [(R, R, R)] -> String
mk_gradient_chart gs =
    let f = P.printf "%.1f"
        gs' = map (\(g,c,v) -> [f g, f c, f v]) gs
    in mk_chart ["gradient", "velocity", "power"] gs'

mk_gearing :: (Double, Double, Double) -> [(G.Gear, Double, Double)]
mk_gearing (c_min, c_max, v) =
  let t_23_622 = G.Tyre 23 622
      cw = [39,53]
      cs_12_25 = [12,13,14,15,16,17,19,21,23,25]
      gs = [G.Gear c s | c <- cw, s <- cs_12_25]
      valid_c c = c >= c_min && c <= c_max
      rs = [(g, G.cadence t_23_622 g v, v) | g <- gs]
  in filter (\(_,c,_) -> valid_c c) rs

mk_gearing_chart :: [(G.Gear, Double, Double)] -> String
mk_gearing_chart gs =
    let f = P.printf "%.1f"
        gs' = map (\(g,c,v) -> [show g, f c, f v]) gs
    in mk_chart ["gear", "cadence", "velocity"] gs'

mk_cadence :: Double -> [(G.Gear, Double, Double)]
mk_cadence c =
  let t_23_622 = G.Tyre 23 622
      cw = [39,53]
      cs_12_25 = [12,13,14,15,16,17,19,21,23,25]
      gs = [G.Gear r s | r <- cw, s <- cs_12_25]
      cmp (_,_,x) (_,_,y) = compare x y
  in L.sortBy cmp [(g, c, G.velocity t_23_622 g c) | g <- gs]
