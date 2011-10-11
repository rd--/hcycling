module Cycling.Plot where

import qualified Data.Colour as C {- colour -}
import Graphics.Rendering.Chart hiding (c) {- chart -}
import Graphics.Rendering.Chart.Gtk

-- | A synonym for 'Double'.
type R = Double

-- | Generate a point chart.
--
-- > mk_chart (100,100) Nothing [mk_plot_pt "pt" C.black 0 [3,0,6]]
mk_plot_pt :: String -> C.Colour R -> R -> [R] -> Plot R R
mk_plot_pt t clr xn y =
  let x = [1+xn,2+xn..]
      v = zip x y
      s = filledCircles 2 (C.opaque clr)
      c = defaultPlotPoints {plot_points_title_ = t
                            ,plot_points_values_ = v
                            ,plot_points_style_ = s}
  in toPlot c

-- | Generate an error value chart.  The triples are /(lower,value,upper)/.
--
-- > let p = mk_plot_tr "tr" C.black 0 [(2,3,4),(3,7,14)]
-- > in mk_chart (100,100) Nothing [p]
mk_plot_tr :: String -> C.Colour R -> R -> [(R,R,R)] -> Plot R R
mk_plot_tr t clr xn y =
  let x = map (\i -> ErrValue (i - 0.01) i (i + 0.01)) [1+xn,2+xn..]
      y' = map (\(i,j,k) -> ErrValue i j k) y
      v = zipWith ErrPoint x y'
      s = solidLine 1 (C.opaque clr)
      c = defaultPlotErrBars {plot_errbars_title_ = t
                             ,plot_errbars_line_style_ = s
                             ,plot_errbars_values_ = v}
  in toPlot c

-- | Generate a line set chart.  The duples are /(x,y)/.
--
-- > let p = mk_plot_ln "ln" C.black 0 [[(2,3),(3,3)],[(7,14),(9,11)]]
-- > in mk_chart (100,100) Nothing [p]
mk_plot_ln :: String -> C.Colour R -> R -> [[(R,R)]] -> Plot R R
mk_plot_ln t clr xn ln =
  let s = solidLine 1 (C.opaque clr)
      c = defaultPlotLines {plot_lines_title_ = t
                           ,plot_lines_style_ = s
                           ,plot_lines_values_ = ln}
  in toPlot c

-- | Generate a rendering of a set of 'Plot's.
mk_chart :: (PlotValue x,PlotValue y) =>
            (Int,Int) -> Maybe FilePath -> [Plot x y] -> IO ()
mk_chart (w,h) fn p = do
  let l = defaultLayout1 {layout1_plots_ = map Left p}
      r = toRenderable l
  case fn of
    Just fn' -> renderableToPDFFile r w h fn'
    Nothing -> renderableToWindow r w h
