module Cycling.Chart (OPT
                     ,cadence_opt,mk_cadence_chart
                     ,cadence_tyre_opt,mk_cadence_tyre_chart
                     ,gearing_cadence_opt,mk_gearing_cadence_chart
                     ,gearing_measurements_opt,mk_gearing_measurements_chart
                     ,gradient_opt,mk_gradient_chart
                     ,vam_opt,mk_vam_chart
                     ,avg_vel_opt,mk_avg_vel_chart
                     ,mk_index) where

import qualified Data.Function as F
import qualified Data.List as L
import qualified Data.List.Split as S
import qualified Data.Maybe as M
import qualified Text.Printf as P
import qualified Text.HTML.Light as H
import qualified Text.XML.Light as X

import qualified Cycling.Cassette as C
import qualified Cycling.Gearing as G
import qualified Cycling.Power as P
import qualified Cycling.Time as T
import qualified Cycling.VAM as VAM
import qualified Cycling.Velocity as V

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
type VAR = (String,String)
type OPT = [VAR]

read_maybe :: (Read a) => String -> Maybe a
read_maybe s =
    case reads s of
      [(i,"")] -> Just i
      _ -> Nothing

read_r :: String -> R
read_r x = maybe 0.0 id (read_maybe x)

unR :: VAR -> R
unR = read_r . snd

unI :: VAR -> Int
unI = floor . unR

read_l :: (Read a) => String -> [a]
read_l s =
    let xs = S.sepBy "," s
    in M.mapMaybe read_maybe xs

unL :: Read a => VAR -> [a]
unL = read_l . snd

unRl :: VAR -> [R]
unRl = unL

unTY :: VAR -> G.Tyre
unTY = G.read_iso_tyre . snd

{-
show_r :: R -> String
show_r x =
    let (i,f) = properFraction x :: (Integer,R)
    in if f == 0.0 then show i else show x

show_l :: [R] -> String
show_l xs = "[" ++ L.intercalate "," (map show_r xs) ++ "]"
-}

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
        sb = H.input [H.class' "submit"
                     ,H.type' "submit"
                     ,H.value "calculate"]
    in H.form
         [H.action "./", H.method "get"]
         (map mk_h z ++ map mk_s o ++ [sb])

gradient_opt :: OPT
gradient_opt =
    [("tolerance", "0.05")
    ,("rider-weight", "62")
    ,("bicycle-weight", "8")
    ,("kit-weight", "2")
    ,("power", "250")
    ,("gradients", "6,7,8,9,10,11,12")]

mk_gradient :: OPT -> [(R, R, R)]
mk_gradient o =
  let [t, m_r, m_b, m_k, w] = map unR (section o (0,4))
      gs = unRl (o !! 5)
      m = m_r + m_b + m_k
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

std_chainrings,std_sprockets :: String
std_chainrings = "39,53"
std_sprockets = C.cassette_string (C.shimano_105_12_25::[Int])

gearing_cadence_opt :: OPT
gearing_cadence_opt =
    [("cadence-minima", "60")
    ,("cadence-maxima", "110")
    ,("velocity", "36")
    ,("chainrings", std_chainrings)
    ,("sprockets", std_sprockets)
    ,("iso-tyre", "23-622")]

section :: [a] -> (Int,Int) -> [a]
section xs (i,j) = take (j - i + 1) (drop i xs)

mk_gearing_cadence :: OPT -> [(G.Gear, Double, Double)]
mk_gearing_cadence o =
  let [c_min, c_max, v] = map unR (section o (0,2))
      [cr, cs] = map unRl (section o (3,4))
      ty = unTY (o !! 5)
      gs = [G.Gear (floor c) (floor s) | c <- cr, s <- cs]
      valid_c c = c >= c_min && c <= c_max
      rs = [(g, G.cadence ty g v, v) | g <- gs]
  in filter (\(_,c,_) -> valid_c c) rs

mk_gearing_cadence_chart :: OPT -> String
mk_gearing_cadence_chart o =
    let f = P.printf "%.1f"
        gs = mk_gearing_cadence o
        gs' = map (\(g,c,v) -> [show g, f c, f v]) gs
        fm = opt_form [("chart","gearing-cadence")] o
    in mk_chart fm ["gear", "cadence", "velocity"] gs'

cadence_opt :: OPT
cadence_opt =
    [("cadences", "90,110")
    ,("chainrings", std_chainrings)
    ,("sprockets", std_sprockets)
    ,("iso-tyre", "23-622")]

mk_cadence :: OPT -> [(G.Gear, Double, Double)]
mk_cadence o =
  let [cd,cr,cs] = map unRl (section o (0,2))
      ty = unTY (o !! 3)
      gs = [G.Gear (floor r) (floor s) | r <- cr, s <- cs]
      cmp (_,_,x) (_,_,y) = compare x y
  in L.sortBy cmp [(g, c, G.velocity ty g c) | c <- cd, g <- gs]

mk_cadence_chart :: OPT -> String
mk_cadence_chart o =
    let f = P.printf "%.1f"
        gs = mk_cadence o
        gs' = map (\(g,c,v) -> [show g, f c, f v]) gs
        fm = opt_form [("chart","cadence")] o
    in mk_chart fm ["gear", "cadence", "velocity"] gs'

cadence_tyre_opt :: OPT
cadence_tyre_opt =
    [("cadence", "90")
    ,("chainring", "53")
    ,("sprockets", "16")
    ,("iso-tyres", "23-622,25-622,28-622")]

mk_cadence_tyre :: OPT -> [(G.Tyre, Double)]
mk_cadence_tyre o =
  let c = unR (o !! 0)
      [cr,cs] = map unI (section o (1,2))
      ty = unL (o !! 3)
      g = G.Gear cr cs
      cmp (_,x) (_,y) = compare x y
  in L.sortBy cmp [(t, G.velocity t g c) | t <- ty]

mk_cadence_tyre_chart :: OPT -> String
mk_cadence_tyre_chart o =
    let f = P.printf "%.1f"
        r = mk_cadence_tyre o
        r' = map (\(t,v) -> [show t, f v]) r
        fm = opt_form [("chart","cadence-tyre")] o
    in mk_chart fm ["tyre", "velocity"] r'

gearing_measurements_opt :: OPT
gearing_measurements_opt =
    [("chainrings", std_chainrings)
    ,("sprockets", std_sprockets)
    ,("iso-tyre", "23-622")]

mk_gearing_measurements :: OPT -> [(G.Gear, Double, Double)]
mk_gearing_measurements o =
  let [cr, cs] = map unRl (section o (0,1))
      ty = unTY (o !! 2)
      gs = [G.Gear (floor c) (floor s) | c <- cr, s <- cs]
      f = L.sortBy (compare `F.on` (\(_,x,_) -> x))
  in f [(g, G.gear_metres ty g, G.gear_inches ty g) | g <- gs]

mk_gearing_measurements_chart :: OPT -> String
mk_gearing_measurements_chart o =
    let f = P.printf "%.1f"
        gs = mk_gearing_measurements o
        gs' = map (\(g,c,v) -> [show g, f c, f v]) gs
        fm = opt_form [("chart","gearing-measurements")] o
    in mk_chart fm ["gear", "metres", "inches"] gs'

vam_opt :: OPT
vam_opt =
    [("vertical-ascension", "600")
    ,("time", "20")
    ,("average-gradient", "8")
    ,("starting-altitude", "0")]

mk_vam :: OPT -> (Double, Double)
mk_vam o =
  let [va,t,gr,a] = map unR o
      vmh = VAM.vam va t
      wkg = VAM.vam_to_power vmh a gr
  in (vmh,wkg)

mk_vam_chart :: OPT -> String
mk_vam_chart o =
    let f = P.printf "%.1f"
        (vmh,wkg) = mk_vam o
        fm = opt_form [("chart","velocita-ascensionale-media")] o
    in mk_chart fm ["vm/h", "watts/kg"] [[f vmh,f wkg]]

avg_vel_opt :: OPT
avg_vel_opt =
    [("distance", "30.0")
    ,("times", "0:30:00.00")]

mk_avg_vel :: OPT -> [(T.Duration,Double)]
mk_avg_vel o =
  let ds = read (snd (o !! 0))
      ts = unL (o !! 1)
      ks = map (V.kph ds) ts
  in zip ts ks

mk_avg_vel_chart :: OPT -> String
mk_avg_vel_chart o =
    let f = P.printf "%.1f"
        tv = mk_avg_vel o
        fm = opt_form [("chart","average-velocity")] o
        g (t,v) = [show t,f v,f (V.kph_to_mph v)]
    in mk_chart fm ["tm","kph","mph"] (map g tv)

mk_index :: String
mk_index =
    let cs = L.sort ["cadence"
                    ,"cadence-tyre"
                    ,"gearing-cadence"
                    ,"gearing-measurements"
                    ,"gradient"
                    ,"velocita-ascensionale-media"
                    ,"average-velocity"]
        mk_ln c = H.li [] [H.a [H.href ("?chart="++c)] [H.cdata c]]
        hd = H.head [] [H.title [] [H.cdata "cycling"], css]
        bd = H.body [] [H.ul [] (map mk_ln cs)]
        h = H.html std_html_attr [hd, bd]
    in H.renderXHTML H.xhtml_1_0_strict h
