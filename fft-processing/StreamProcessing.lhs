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
import Foreign.Marshal.Array (newArray, peekArray)
import Data.Complex
import qualified Data.Array.IArray as Arr
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Int (Int16)
-- import Math.FFT
import Numeric.FFT

import PlaySine -- (openPCM, closePCM, playSound, playBuffer)
import Plotting
import FFTProcessing

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Lens


\end{code}
\end{comment}

\begin{code}

bufferSize = 1024


-- use hGetBuffer ... yeah, didn't this cause problems earlier???
main :: IO ()
main = do
  -- I think this opens a sine wave file...
  fileh <- Snd.openFile "test.wav" Snd.ReadMode Snd.defaultInfo


  -- this shouldn't be necessary, as long as the crashing doesn't happen with ghc
  let 
    info = Snd.hInfo fileh
    frames = Snd.frames info
  print frames

  -- this stuff needs to happen in some kind of a loop
--   ptr <- newArray $ replicate 1024 0 -- (0 :: Int)
--   ptrMaybe <- peekArray frames ptr
--   -- print ptrMaybe
--   -- read <- Snd.readFile "accordionlow"

--   size <- Snd.hGetBuf fileh ptr (frames)

--   -- if I understand correctly, closing the file when it wasn't finished
--   -- reading was part of the problem
--   -- don't actually just return the list of [Int16]
  processFile fileh -- frames
  Snd.hClose fileh

-- main = print "hello"


-- since main has the file pointer, put the rest in a different function
processFile h = do
  ptr <- newArray $ replicate bufferSize (0::Int16)
  size <- Snd.hGetBuf h ptr bufferSize

  print "iterating"
  buffer <- peekArray bufferSize ptr
  -- process the buffer
  
  print buffer


  if size < bufferSize
    then do
      print "done"
    else processFile h

\end{code}

\end{document}