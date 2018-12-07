\documentclass[a4paper]{article}

\usepackage[english]{babel}
\usepackage[utf8]{inputenc}
\usepackage{amsmath}
\usepackage{graphicx}
\usepackage{comment}
\usepackage{cite}
\usepackage{url}
\usepackage{hyperref} % for inserting the link
\usepackage{calculation}
\usepackage{verbatim} % for multiline comments
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{color}
\usepackage{multicol}
\usepackage{enumitem} % for alpha enumerations
\usepackage{listings} % for putting code snippets
\lstset{
  breaklines=true % line breaks for listings
}
\usepackage{cancel} % for crossing out cancellations in math

\lstnewenvironment{code}     %defining the code environment
  {\lstset{
    language=Haskell,
    basicstyle=\small\ttfamily,
    breaklines=true,             % for code to break at end of line
    literate={•}{{$\bullet$}}1,  % defining the bullet
    }}
  {}

\begin{document}

\begin{comment}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import qualified Graphics.UI.FLTK.LowLevel.FL as FL
-- import Graphics.UI.FLTK.LowLevel.Fl_Types
-- import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Sound.File.Sndfile as Snd
import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import GHC.Float (double2Float)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar, swapMVar, newMVar, MVar)
import Control.Monad (forever)
-- import Data.Array.CArray
import Data.Complex
import qualified Data.Array.IArray as Arr
-- import Math.FFT
import Numeric.FFT

import PlaySine -- (openPCM, closePCM, playSound, playBuffer)

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens


\end{code}
\end{comment}

-- using this transform
-- dftRH :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r

-- this means figuring out CArray
-- based on the constraints on the CArray parameters, it looks like we're dealing
-- with CArray Int Double
-- (hopefully this doesn't cause problems with the Int16 PCM representations...)

-- https://hackage.haskell.org/package/array-0.5.1.0/docs/Data-Array-IArray.html
\begin{code}
-- the frequency, in Hz
type Pitch = Double
type Wave = Double -> Double


frameRate = 44100
\end{code}
-- think about the frequency of this sin function....
-- when taking the 

In order to produce a wave sample, the wave function is applied to an initial sequence.
This initial sequence of numbers is spaced by the number of oscillations per frame.
Given the pitch in Hz, we perform the following calculation

\[\textsf{pitch } \frac{\textsf{oscillations}}{\cancel{\textsf{second}}}
* \frac{\cancel{\textsf{second}}}{44100 \textsf{ frame}}
= \frac{\textsf{oscillations}}{\textsf{frame}} \]

\begin{code}
waveSample :: Double -> Wave -> Pitch -> Snd.Count -> [Double]
waveSample amp wave pitch sampleLength =
  let
    oscillationsPerFrame = pitch/frameRate
  in
    map ((amp*) . wave . (*(oscillationsPerFrame*2*pi)) . fromIntegral) [0..sampleLength]
  -- map ((2330*) . wave) [0,0.1..(fromIntegral sampleLength)]


-- playback of the sin and square at 440 hz
-- playBuffer $ return $ map round $ waveSample 3000 square 440 100000
-- playBuffer $ return $ map round $ waveSample 20000 sin 440 100000


  -- do this part later
    -- Arr.listArray (0,sampleLength-1) list




-- the square sample should be the exact same function, except replaced with a square function
\end{code}

Normalized to have a period of $2\pi$, just like $\sin$.
\begin{code}
square :: Double -> Double
square x =
  if 0.5 > snd (properFraction $ x/(2*pi))
    then 1
    else -1


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


sinChart = toRenderable layout
  where
    wave = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (zip [(0::Double)..] (waveSample 1 sin 440 2000))
              $ plot_points_title .~ "sin"
              $ def
    layout = layout_title .~ "Sine Wave"
           $ layout_plots .~ [toPlot wave]
           $ def

sineChartImg = renderableToFile def "sine.png" sinChart

plot title d = renderableToFile def (title ++ ".png") chart
  where
    chart = toRenderable layout
    wave = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (zip [(0::Double)..] d)
              $ plot_points_title .~ title
              $ def
    layout = layout_title .~ title
           $ layout_plots .~ [toPlot wave]
           $ def

sinFFT = map magnitude $ dft $ map (:+ 0) $ waveSample 1 sin 440 2000

squareFFT = map imagPart $ dft $ map (:+ 0) $ waveSample 1 square 440 2000

main = print "Hello World!"
\end{code}

\end{document}