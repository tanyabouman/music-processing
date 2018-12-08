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
module FFTProcessing where
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
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
-- import Math.FFT
import Numeric.FFT

import PlaySine -- (openPCM, closePCM, playSound, playBuffer)
import Plotting

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
\end{code}

This function does all of the processing to make the fft happen.
Since all of the inputs are real values, we just take in doubles,
and only keep the first half of the results, since the rest is
a mirror image.
\begin{code}
myDFT :: [Double] -> [Double]
myDFT xs =
  let
    complex = map (:+ 0) xs
    halfdft = take ((length xs) `div` 2) $ dft complex
    ans = map magnitude halfdft
  in
    ans
\end{code}


Given the (half) Fourier transform of a signal, we look for the peak frequency.
This also depends on the globally defined frame rate, 44100, since the frame
rate determines the value of the Nyquist frequency.
\begin{code}
findFrequency :: [Double] -> Double
findFrequency ft =
  let
    l = length ft
    m = maximum ft
    -- lookup the index of the maximum
    -- using Eq on doubles is OK here, because there is no calculation
    ix = fromMaybe 0 $ elemIndex m ft
    (ix1,ix2,ix3)
      | ix == 0 = (0,1,2)
      | ix == l-1 = (l-3,l-2,l-1)
      | otherwise = (ix-1,ix,ix+1)
    (x1,x2,x3) = (fromIntegral ix1, fromIntegral ix2, fromIntegral ix3)
    (y1,y2,y3) = (ft !! ix1, ft !! ix2, ft !! ix3)
    -- there are now three points for the interpolation
    p x = (x*x - (x2+x3)*x + x2*x3)*y1/2
        - (x*x - (x1+x3)*x + x1*x3)*y2
        + (x*x - (x1+x2)*x + x1*x2)*y3/2
    -- find the peak a modified binary search
    search p low high =
      let mid = (low+high)/2
      in
        if low + 1e-12 > high
        then mid
        else if p low < p high
        then search p mid high
        else search p low mid
    maxIdx = search p x1 x3
  in
    maxIdx * frameRate / (2*(fromIntegral l))



sinFFT = myDFT $ waveSample 1 sin 440 2000

squareFFT = myDFT $ waveSample 1 square 440 2000

inputFreq = [200,400..24000]
measuredFreq f = findFrequency $ myDFT $ waveSample 1 sin f 2000
measurements = map measuredFreq inputFreq

measurementChart = plot "Measured Frequencies" inputFreq measurements
differenceChart = plot "Measured Frequency Differences" inputFreq (zipWith (-) inputFreq measurements)


sinChart = plot "Sine" [(0::Double)..] (waveSample 1 sin 440 2000)


\end{code}
Show that the frequencies work for the most part, except past Nyquist
Show the calculated frequency differences


On to the low pass filter.

Pick n to be 2 for now.  Why not?
Is this supposed to be a subtraction???
What is going on?
\begin{code}

frequencyBins = [0,frameRate/2000..]


g s = 1 / sqrt (1+s**2)

-- this does the filter on the sine and plots it.
-- not that interesting, since it's only a sine
-- plot "filteredSin330g10" frequencyBins $ zipWith (*) sinFFT $ take 100 (map (g . (/330)) frequencyBins)

--putting all the low pass things together
lowPass :: Double -> [Double] -> [Double]
lowPass cutoff input = zipWith (*) input $ map (g . (/cutoff)) frequencyBins



-- interesting results
-- plot "Hann" [(0::Double)..] $ hannWindow 1200 $ waveSample 1 sin 440 2000
-- multiply the time domain with a Hann window
hannWindow :: Double -> [Double] -> [Double]
hannWindow windowSize input =
  let
    hann n = 0.5 * (1 - cos(2*pi*n/(windowSize-1)))
  in
    zipWith (*) input $ map hann frequencyBins



-- time domain and fast convolution


-- well, this is interesting, so don't apply the magnitude before doing the inverse

-- this gives back the original, but with twice as much information
-- plot "SineFFTed" [(0::Double)..] $ map magnitude $ idft $ dft $ map (:+0) $ waveSample 1 sin 440 2000
-- *Main> plot "SineFFTedhalf" [(0::Double)..] $ map magnitude $ idft $ map (:+ 0) $ myDFT $ waveSample 1 sin 440 2000

-- also weird
-- plot "SineFFTed" [(0::Double)..] $ map magnitude $ idft $ take 1000 $ dft $ map (:+0) $ waveSample 1 sin 440 2000


-- phase shifted, but at least reasonable
-- try this with a square wave to check that it reasonably works out
-- plot "SineFFTedreal" [(0::Double)..] $ map realPart $ idft $ dft $ map (:+0) $ waveSample 1 sin 440 2000

-- basically nonsense, and reasonably zero, which makes sense, but the patterns in it probably produced the strange magnitude output
-- plot "SineFFTedreal" [(0::Double)..] $ map realPart $ idft $ dft $ map (:+0) $ waveSample 1 sin 440 2000



-- takes a function which represents the convolution and the input
-- the convolution function operations on the frequency themselves
-- as listed above in the frequencyBins
fastConvolution :: (Double -> Double) -> [Double] -> [Double]
fastConvolution conv input =
  let
    ffted = dft $ map (:+0) $ input
    conved = zipWith mult ffted $ map conv frequencyBins
    iffted = map realPart $ idft conved
  in
    iffted


mult :: Complex Double -> Double -> Complex Double
mult (y :+ z) x = x*y :+ x*z


-- main = print "Hello World!"
\end{code}

\end{document}