  -- modified from https://github.com/deech/fltkhs-hello-world

{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import GHC.Float (double2Float)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar, swapMVar, newMVar, MVar)
import Control.Monad (forever)

import PlaySine -- (openPCM, closePCM, playSound)


buttonCb :: Ref Button -> IO ()
buttonCb b' = do
  l' <- getLabel b'
  if (l' == "Play")
    then setLabel b' "Pause"
    else setLabel b' "Play"

playTone :: {- (PCM.Access i, PCM.SampleFmt y) => (PCM.Size, PCM.SampleFreq, PCM.Handle PCM.Interleaved Float) -> -} MVar Double -> IO ()
playTone freq = do
  forkIO (forever $ do
    p <- readMVar freq
    settings <- openPCM
    playSound settings ((double2Float p){-*100+400-}) 1
    closePCM settings
    )
  return ()

changeFreq :: MVar Double -> Ref HorValueSlider -> IO ()
changeFreq freq s = do
  p <- getValue s
  old <- swapMVar freq p
  return ()


ui :: IO ()
ui = do

  window <- windowNew
           (Size (Width 515) (Height 60))
           Nothing
           Nothing
  begin window
  b' <- buttonNew
        (Rectangle (Position (X 10) (Y 10)) (Size (Width 95) (Height 30)))
        (Just "Play")
  setLabelsize b' (FontSize 10)
  setCallback b' buttonCb

  s <- horValueSliderNew -- valueSliderNew 
        (Rectangle (Position (X 120) (Y 10)) (Size (Width 350) (Height 30)))
        (Just "Pitch")
  setLabelsize s (FontSize 10)
  setMinimum s 330
  setMaximum s 770
  setValue s 330
  freq <- newMVar 330
  playTone freq
  setCallback s (changeFreq freq)

  end window
  showWidget window

main :: IO ()
main = ui >> FL.run >> FL.flush

-- run is the thing that sets up the fl window.  fltk already runs on a separate thread

replMain :: IO ()
replMain = ui >> FL.replRun
