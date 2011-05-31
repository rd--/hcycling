import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import System.FilePath

import qualified Cycling.Power as P

type R = Double

mk_plot_ln :: String -> Colour R -> [(R,R)] -> Plot R R
mk_plot_ln t clr v =
    let s = solidLine 1 (opaque clr)
        c = defaultPlotLines {plot_lines_title_ = t
                             ,plot_lines_values_ = [v]
                             ,plot_lines_style_ = s}
    in toPlot c

mk_chart :: (Int,Int) -> Maybe FilePath -> [Plot R R] -> IO ()
mk_chart (w,h) fn p = do
  let l = defaultLayout1 {layout1_plots_ = map Left p}
      r = toRenderable l
  case fn of
    Just fn' -> case takeExtension fn' of
                  "pdf" -> renderableToPDFFile r w h fn'
                  "svg" -> renderableToSVGFile r w h fn'
                  _ -> error "mk_chart: unknown extension"
    Nothing -> renderableToWindow r w h

clr_cyc :: [Colour R]
clr_cyc = cycle [black,blue,red,yellow,green,orange,cyan,magenta]

gradient_v_tbl :: (R,R,R) -> [R] -> [R]
gradient_v_tbl (tolerance,mass,power) =
    let f g = fst (P.velocity_std tolerance mass g power)
    in map f

gradient_v_chart :: Maybe FilePath -> (R,R) -> [R] -> [R] -> IO ()
gradient_v_chart fn (t,w) m g = do
  let l = map (\m' -> show (w,m')) m
      v = map (\m' -> gradient_v_tbl (t,m',w) g) m
      p = zipWith3 mk_plot_ln l clr_cyc (map (zip g) v)
  mk_chart (1920,1080) fn p

gradient_p_tbl :: (R,R) -> [R] -> [R]
gradient_p_tbl (mass,velocity) =
    let f g = P.power_std mass g velocity
    in map f

gradient_p_chart :: Maybe FilePath -> R -> [R] -> [R] -> IO ()
gradient_p_chart fn v m g = do
  let l = map (\m' -> show (v,m')) m
      w = map (\m' -> gradient_p_tbl (m',v) g) m
      p = zipWith3 mk_plot_ln l clr_cyc (map (zip g) w)
  mk_chart (1920,1080) fn p

{-
gradient_v_tbl (0.05,63+9,250) [0,0.5 .. 18]
gradient_v_chart Nothing (0.05,250) [65,70 .. 100] [-5,-4.5 .. 20]
gradient_p_chart Nothing 20 [65,70 .. 100] [0,0.5 .. 20]
-}
