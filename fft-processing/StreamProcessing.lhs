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
import Foreign.Marshal.Array (newArray, peekArray, pokeArray)
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

format :: Snd.Format
format = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile



-- use hGetBuffer ..., didn't this cause problems earlier??? only in ghci, not ghc
main :: IO ()
main = do
  -- I think this opens a sine wave file...
  -- open both the input and the output file
  inputH <- Snd.openFile "test2.wav" Snd.ReadMode Snd.defaultInfo
  -- hopefully defaultinfo is OK for this????
  let info = Snd.Info (bufferSize `div` 2) 44100 1 format 1 False
  outputH <- Snd.openFile "test4.wav" Snd.WriteMode info

  -- this shouldn't be necessary, as long as the crashing doesn't happen with ghc
  let 
    info = Snd.hInfo inputH
    frames = Snd.frames info
  print frames

  processFile inputH outputH calculation
  Snd.hClose inputH
  Snd.hClose outputH

-- main = print "hello"

-- this should be passed through the processFile
-- could be a Hann window, fft, whatever
calculation :: [Int16] -> [Int16]
calculation = map round . hannWindow (fromIntegral bufferSize) . map fromIntegral
-- calculation = id




processFile inputH outputH calculation = do
  let halfBuffer = bufferSize `div` 2
  ptr <- newArray $ replicate halfBuffer (0::Int16)
  size <- Snd.hGetBuf inputH ptr halfBuffer

  let
    processFile' oldBuffer oldBufferP = do
      size <- Snd.hGetBuf inputH ptr halfBuffer

      print "iterating"
      newBuffer <- peekArray halfBuffer ptr
      let
        buffer = oldBuffer ++ newBuffer
        -- process the buffer
        bufferP = calculation buffer
        (first,second) = splitAt halfBuffer bufferP
        toWrite = zipWith (+) first oldBufferP

      -- print buffer

      pokeArray ptr toWrite
      -- FIXME: this will write zeroes past the end of the file
      c <- Snd.hPutBuf outputH ptr halfBuffer

      -- write back???

      if size < halfBuffer
        then do
          c <- Snd.hPutBuf outputH ptr halfBuffer
          print "done"
        else processFile' newBuffer second

  initBuffer <- peekArray halfBuffer ptr

  processFile' initBuffer (calculation initBuffer)


-- since main gets the file pointer, put the rest in a different function
-- nice so far, but the buffers need to overlap...hmmmm
-- the oldBuffer is the second half of the previous buffer
  

\end{code}

\end{document}