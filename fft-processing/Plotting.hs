{-# LANGUAGE OverloadedStrings #-}
module Plotting where
-- import qualified Graphics.UI.FLTK.LowLevel.FL as FL
-- import Graphics.UI.FLTK.LowLevel.Fl_Types
-- import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Sound.File.Sndfile as Snd
import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import GHC.Float (double2Float)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar, swapMVar, newMVar, MVar)
import Control.Monad (forever)
import Foreign.Marshal.Array (newArray, peekArray)
import Data.Complex
import qualified Data.Array.IArray as Arr
import Data.Int (Int16)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
-- import Math.FFT
import Numeric.FFT

import PlaySine -- (openPCM, closePCM, playSound, playBuffer)

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens

plot :: String -> [Double] -> [Double] -> IO (PickFn ())
plot title domain range = renderableToFile def (title ++ ".png") chart
  where
    chart = toRenderable layout
    wave = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (zip domain range)
              $ plot_points_title .~ title
              $ def
    layout = layout_title .~ title
           $ layout_plots .~ [toPlot wave]
           $ def


setLinesBlue :: PlotLines a b -> PlotLines a b
setLinesBlue = plot_lines_style  . line_color .~ opaque blue

chart = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))

    sinusoid1 = plot_lines_values .~ [[ (x,(am x)) | x <- [0,(0.5)..400]]]
              $ plot_lines_style  . line_color .~ opaque blue
              $ plot_lines_title .~ "am"
              $ def

    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ [ (x,(am x)) | x <- [0,7..400]]
              $ plot_points_title .~ "am points"
              $ def

    layout = layout_title .~ "Amplitude Modulation"
           $ layout_plots .~ [toPlot sinusoid1,
                              toPlot sinusoid2]
           $ def

exampleChart1 = renderableToFile def "example1_big.png" chart


  -- toRenderable layout
  -- where
  --   wave = plot_points_style .~ filledCircles 2 (opaque red)
  --             $ plot_points_values .~ (zip [(0::Double)..] (waveSample 1 sin 440 2000))
  --             $ plot_points_title .~ "sin"
  --             $ def
  --   layout = layout_title .~ "Sine Wave"
  --          $ layout_plots .~ [toPlot wave]
  --          $ def

-- sineChartImg = renderableToFile def "sine.png" sinChart


frameRate :: Double
frameRate = 44100

plotWavFile :: FilePath -> IO (PickFn ())
plotWavFile fp = do
  -- get the file information
  fileh <- Snd.openFile fp Snd.ReadMode Snd.defaultInfo
  let
    info = Snd.hInfo fileh
    frames = Snd.frames info
  print frames
  ptr <- newArray $ replicate frames (0::Int16)
  size <- Snd.hGetBuf fileh ptr frames
  listPtr <- peekArray frames ptr
  Snd.hClose fileh

  -- plot it
  plot fp [0{- (fromIntegral frames / frameRate)-}..] 
             (map fromIntegral listPtr)


