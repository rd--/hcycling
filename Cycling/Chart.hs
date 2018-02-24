module Cycling.Chart (OPT
                     ,cadence_opt,mk_cadence_chart
                     ,cadence_tyre_opt,mk_cadence_tyre_chart
                     ,gearing_cadence_opt,mk_gearing_cadence_chart
                     ,gearing_measurements_opt,mk_gearing_measurements_chart
                     ,gradient_opt,mk_gradient_chart
                     ,vam_opt,mk_vam_chart
                     ,avg_vel_opt,mk_avg_vel_chart
                     ,et_cmp_opt,mk_et_cmp_chart
                     ,mk_index) where

import qualified Data.Function as F {- base -}
import qualified Data.List as L {- base -}
import qualified Data.List.Split as S {- split -}
import qualified Data.Maybe as M {- base -}
import qualified Text.Printf as P {- base -}
import qualified Text.XML.Light as X {- xml -}

import qualified Text.HTML.Minus as H {- html-minus -}
import qualified Music.Theory.Time.Duration as T {- hmt -}

import qualified Cycling.Cassette as C
import qualified Cycling.Gearing as G
import qualified Cycling.Power as P
import qualified Cycling.VAM as VAM
import qualified Cycling.Velocity as V

std_html_attr :: [X.Attr]
std_html_attr = [H.lang "en" ]

std_meta :: [X.Content]
std_meta =
    [H.title [] [H.cdata "cycling"]
    ,H.meta_author "rohan drape"
    ,H.meta_content_type "text/html; charset=UTF-8"
    ,H.meta_viewport "width=device-width,initial-scale=1,user-scalable=yes"
    ,H.link_css "all" "css/cycling.css"]

mk_chart_c :: X.Content -> [String] -> [[(String,Maybe String)]] -> String
mk_chart_c fm t g =
    let mk_cl (e,c) = let a = case c of
                                Just c' -> [H.class_attr c']
                                Nothing -> []
                      in H.td a [H.cdata e]
        mk_tr xs = H.tr [] (map mk_cl xs)
        th = H.tr [] (map (\x -> H.th [] [H.cdata x]) t)
        hd = H.head [] std_meta
        bd = H.body [] [fm,H.table [H.class_attr "result"] (th : map mk_tr g)]
        h = H.html std_html_attr [hd,bd]
    in H.renderHTML5 h

mk_chart :: X.Content -> [String] -> [[String]] -> String
mk_chart fm t = mk_chart_c fm t . map (map (\e -> (e,Nothing)))

type R = Double
type Name = String
type Default = String
type Unit = String
data Mode = Atom | List
type VAR = (Name,Default,Unit,Mode)
type OPT = [VAR]

var_n :: VAR -> String
var_n (_,n,_,_) = n

read_maybe :: (Read a) => String -> Maybe a
read_maybe s =
    case reads s of
      [(i,"")] -> Just i
      _ -> Nothing

read_r :: String -> R
read_r x = M.fromMaybe 0.0 (read_maybe x)

unD :: VAR -> T.Duration
unD = read . var_n

unR :: VAR -> R
unR = read_r . var_n

--unI :: VAR -> Int
--unI = floor . unR

read_l :: (Read a) => String -> [a]
read_l s =
    let xs = S.splitOn "," s
    in M.mapMaybe read_maybe xs

unL :: Read a => VAR -> [a]
unL = read_l . var_n

unRl :: VAR -> [R]
unRl = unL

unTY :: VAR -> G.Tyre
unTY = G.read_iso_tyre . var_n

section :: [a] -> (Int,Int) -> [a]
section xs (i,j) = take (j - i + 1) (drop i xs)

{-
show_r :: R -> String
show_r x =
    let (i,f) = properFraction x :: (Integer,R)
    in if f == 0.0 then show i else show x

show_l :: [R] -> String
show_l xs = "[" ++ L.intercalate "," (map show_r xs) ++ "]"
-}

mode_str :: Mode -> String
mode_str m =
    case m of
      Atom -> ""
      List -> "{}"

append_mode_str :: String -> Mode -> String
append_mode_str s m =
    case mode_str m of
      [] -> s
      m' -> s ++ " " ++ m'

opt_form :: (String,String) -> OPT -> X.Content
opt_form z o =
    let mk_h (k,v) = H.input [H.type_attr "hidden"
                             ,H.name k
                             ,H.value v]
        mk_s (k,v,u,m) =
            H.tr [] [H.td [H.class_attr "key"]
                          [H.cdata (append_mode_str k m)]
                    ,H.td []
                          [H.input
                                [H.type_attr "text"
                                ,H.name k
                                ,H.value v]]
                    ,H.td [H.class_attr "unit"]
                          [H.cdata u]]
        sb = H.input [H.class_attr "submit"
                     ,H.type_attr "submit"
                     ,H.value "calculate"]
    in H.form
         [H.action "./",H.method "get"]
         (mk_h z : H.table [H.class_attr "opt-form"] (map mk_s o) : [sb])

gradient_opt :: OPT
gradient_opt =
    [("tolerance","0.05","",Atom)
    ,("rider-weight","63.5,70","kg",List)
    ,("bicycle-weight","8","kg",Atom)
    ,("kit-weight","2","kg",Atom)
    ,("power","280,320","w",List)
    ,("gradient","6,7,8,9,10,11,12","%",List)]

mk_gradient :: OPT -> [(R,R,R,R)]
mk_gradient o =
  let [t,m_b,m_k] = map (unR . (o !!)) [0,2,3]
      m_rl = unRl (o !! 1)
      wl = unRl (o !! 4)
      gl = unRl (o !! 5)
      f (m_r,w,g) = let m = m_r + m_b + m_k
                        (v,w') = P.velocity_std t m g w
                    in (m_r,g,v,w')
  in [f (m_r,w,g) | m_r <- m_rl, w <- wl, g <- gl]

mk_gradient_chart :: OPT -> String
mk_gradient_chart o =
    let f = P.printf "%.1f"
        gs = mk_gradient o
        gs' = map (\(r,g,c,v) -> [f r,f g,f c,f v]) gs
        fm = opt_form ("chart","gradient") o
    in mk_chart fm ["rider-weight","gradient","velocity","power"] gs'

std_chainrings,std_sprockets :: String
std_chainrings = "39,53"
std_sprockets = C.cassette_string (C.shimano_105 (12,25) :: [Int])

gearing_cadence_opt :: OPT
gearing_cadence_opt =
    [("cadence-minima","60","rpm",Atom)
    ,("cadence-maxima","120","rpm",Atom)
    ,("velocity","36","kph",Atom)
    ,("chainring",std_chainrings,"",List)
    ,("sprocket",std_sprockets,"",List)
    ,("iso-tyre","23-622","iso",Atom)]

mk_gearing_cadence :: OPT -> [(G.Gear,Double,Double)]
mk_gearing_cadence o =
  let [c_min,c_max,v] = map unR (section o (0,2))
      [cr,cs] = map unRl (section o (3,4))
      ty = unTY (o !! 5)
      gs = [G.Gear (floor c) (floor s) | c <- cr,s <- cs]
      valid_c c = c >= c_min && c <= c_max
      rs = [(g,G.cadence ty g v,v) | g <- gs]
  in filter (\(_,c,_) -> valid_c c) rs

gear_class :: G.Gear -> String
gear_class g = "cw-" ++ show (G.chainwheel g)

mk_gearing_cadence_chart :: OPT -> String
mk_gearing_cadence_chart o =
    let f = P.printf "%.1f"
        gs = mk_gearing_cadence o
        gs' = map (\(g,c,v) -> [(show g,Just (gear_class g))
                               ,(f c,Nothing)
                               ,(f v,Nothing)]) gs
        fm = opt_form ("chart","gearing-cadence") o
    in mk_chart_c fm ["gear","cadence","velocity"] gs'

cadence_opt :: OPT
cadence_opt =
    [("cadence","90,110","rpm",List)
    ,("chainring",std_chainrings,"",List)
    ,("sprocket",std_sprockets,"",List)
    ,("iso-tyre","23-622","iso",Atom)]

mk_cadence :: OPT -> [(G.Gear,Double,Double)]
mk_cadence o =
  let [cd,cr,cs] = map unRl (section o (0,2))
      ty = unTY (o !! 3)
      gs = [G.Gear (floor r) (floor s) | r <- cr,s <- cs]
      cmp (_,_,x) (_,_,y) = compare x y
  in L.sortBy cmp [(g,c,G.velocity ty g c) | c <- cd,g <- gs]

mk_cadence_chart :: OPT -> String
mk_cadence_chart o =
    let f = P.printf "%.1f"
        gs = mk_cadence o
        gs' = map (\(g,c,v) -> [(show g,Just (gear_class g))
                               ,(f c,Nothing)
                               ,(f v,Nothing)]) gs
        fm = opt_form ("chart","cadence") o
    in mk_chart_c fm ["gear","cadence","velocity"] gs'

cadence_tyre_opt :: OPT
cadence_tyre_opt =
    [("cadence","90","rpm",Atom)
    ,("chainring","53","",List)
    ,("sprocket","16","",List)
    ,("iso-tyre","23-622,25-622,28-622","iso",List)]

mk_cadence_tyre :: OPT -> [(G.Tyre,Double)]
mk_cadence_tyre o =
  let cd = unR (o !! 0)
      [cr,sp] = map unL (section o (1,2))
      ty = unL (o !! 3)
      cmp (_,x) (_,y) = compare x y
  in L.sortBy cmp [(t,G.velocity t (G.Gear r s) cd) |
                   t <- ty,r <- cr, s <- sp]

mk_cadence_tyre_chart :: OPT -> String
mk_cadence_tyre_chart o =
    let f = P.printf "%.1f"
        r = mk_cadence_tyre o
        r' = map (\(t,v) -> [show t,f v]) r
        fm = opt_form ("chart","cadence-tyre") o
    in mk_chart fm ["tyre","velocity"] r'

gearing_measurements_opt :: OPT
gearing_measurements_opt =
    [("chainring",std_chainrings,"",List)
    ,("sprocket",std_sprockets,"",List)
    ,("iso-tyre","28-630","iso",Atom)]

mk_gearing_measurements :: OPT -> [(G.Gear,Double,Double)]
mk_gearing_measurements o =
  let [cr,cs] = map unRl (section o (0,1))
      ty = unTY (o !! 2)
      gs = [G.Gear (floor c) (floor s) | c <- cr,s <- cs]
      f = L.sortBy (compare `F.on` (\(_,x,_) -> x))
  in f [(g,G.gear_metres ty g * 100,G.gear_inches ty g) | g <- gs]

mk_gearing_measurements_chart :: OPT -> String
mk_gearing_measurements_chart o =
    let f = P.printf "%.1f"
        gs = mk_gearing_measurements o
        gs' = map (\(g,c,v) -> [(show g,Just (gear_class g))
                               ,(f c,Nothing)
                               ,(f v,Nothing)]) gs
        fm = opt_form ("chart","gearing-measurements") o
    in mk_chart_c fm ["gear","cm","in"] gs'

vam_opt :: OPT
vam_opt =
    [("vertical-ascension","600","m",Atom)
    ,("time","0:20:00.00","h:m:s",Atom)
    ,("average-gradient","8","%",Atom)
    ,("starting-altitude","0","m",Atom)]

mk_vam :: OPT -> (Double,Double)
mk_vam o =
  case o of
    [va,d,gr,a] -> let vmh = VAM.vam (unR va) (unD d)
                       wkg = VAM.vam_to_power vmh (unR a) (unR gr)
                   in (vmh,wkg)
    _ -> error "mk_vam"

mk_vam_chart :: OPT -> String
mk_vam_chart o =
    let f = P.printf "%.1f"
        (vmh,wkg) = mk_vam o
        fm = opt_form ("chart","velocita-ascensionale-media") o
    in mk_chart fm ["vm/h","w/kg"] [[f vmh,f wkg]]

avg_vel_opt :: OPT
avg_vel_opt =
    [("distance","30.0","km",Atom)
    ,("time","0:30:00.00","h:m:s",List)]

mk_avg_vel :: OPT -> [(T.Duration,Double)]
mk_avg_vel o =
  let ds = read (var_n (o !! 0))
      ts = unL (o !! 1)
      ks = map (V.kph ds) ts
  in zip ts ks

mk_avg_vel_chart :: OPT -> String
mk_avg_vel_chart o =
    let f = P.printf "%.1f"
        tv = mk_avg_vel o
        fm = opt_form ("chart","average-velocity") o
        g (t,v) = [show t,f v,f (V.kph_to_mph v)]
    in mk_chart fm ["tm","kph","mph"] (map g tv)

-- * Elapsed Time Comparison

et_cmp_opt :: OPT
et_cmp_opt =
    [("A","0:30:00.00","h:m:s",List)
    ,("B","0:30:00.00","h:m:s",List)]

mk_et_cmp :: OPT -> [(T.Duration,T.Duration)]
mk_et_cmp o =
  let a = unL (o !! 0)
      b = unL (o !! 1)
  in zip a b

mk_et_cmp_chart :: OPT -> String
mk_et_cmp_chart o =
    let tv = mk_et_cmp o
        fm = opt_form ("chart","elapsed-time-comparison") o
        pc a b = let f = T.duration_to_seconds
                 in P.printf "%.1f" ((f b / f a) * 100 - 100 :: Double)
        g (a,b) = [show a,show b,show (b `T.duration_diff` a),pc a b]
    in mk_chart fm ["A","B","-","%"] (map g tv)

mk_index :: String
mk_index =
    let cs = L.sort ["cadence"
                    ,"cadence-tyre"
                    ,"gearing-cadence"
                    ,"gearing-measurements"
                    ,"gradient"
                    ,"velocita-ascensionale-media"
                    ,"average-velocity"
                    ,"elapsed-time-comparison"]
        mk_ln c = H.li [] [H.a [H.href ("?chart="++c)] [H.cdata c]]
        hd = H.head [] std_meta
        bd = H.body [] [H.ul [] (map mk_ln cs)]
        h = H.html std_html_attr [hd,bd]
    in H.renderHTML5 h
